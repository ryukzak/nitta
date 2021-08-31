import asyncio
import os
import pickle
import subprocess
import time
from dataclasses import dataclass, field
from pathlib import Path
from typing import List, Optional, Any

import pandas as pd
from aiohttp import ClientSession
from cached_property import cached_property
from cachetools import cached
from dataclasses_json import dataclass_json, LetterCase

from modules.utils import log_debug, is_port_in_use

NITTA_PORT = 53829
NITTA_BASEURL = f"http://localhost:{NITTA_PORT}"
METRICS_WEIGHTS = pd.Series(dict(duration=-1, depth=-0.1))
LAMBDA = 0.6
WAIT_NITTA_DELAY = 2

DATA_ROOT = Path("data")

DATA_ROOT.mkdir(exist_ok=True)


def cached_node_method(wrapped):
    return cached({}, key=lambda self, *args: hash(self.sid))(wrapped)


nitta_dataclass_params = dict(letter_case=LetterCase.CAMEL)


@dataclass_json(**nitta_dataclass_params)
@dataclass
class NittaNodeDecision:
    tag: str


@dataclass_json(**nitta_dataclass_params)
@dataclass
class NittaNode:
    score: Optional[int]
    is_terminal: bool
    is_finish: bool
    parameters: Any
    decision: NittaNodeDecision
    duration: Optional[int]
    sid: str

    children: Optional[List['NittaNode']] = field(default=None, repr=False)
    parent: Optional['NittaNode'] = field(default=None, repr=False)

    @property
    def is_leaf(self):
        return self.is_terminal

    @cached_property
    def subtree_size(self):
        assert self.children is not None
        return sum(child.subtree_size for child in self.children) + 1

    @cached_property
    def depth(self) -> int:
        return self.sid.count('-') if self.sid != '-' else 0

    @cached_property
    def subtree_leafs_metrics(self) -> pd.DataFrame:
        if self.is_leaf:
            if not self.is_finish:
                return pd.DataFrame()
            return pd.DataFrame(dict(duration=[self.duration], depth=[self.depth]))
        else:
            return pd.concat([child.subtree_leafs_metrics for child in self.children])

    @cached_node_method
    def get_subtree_leafs_labels(self, metrics_distrib: pd.DataFrame) -> pd.Series:
        if self.is_leaf:
            return pd.Series([self.compute_label(metrics_distrib)])
        else:
            return pd.concat([child.get_subtree_leafs_labels(metrics_distrib) for child in self.children])

    @cached_node_method
    def compute_label(self, metrics_distrib: pd.DataFrame) -> float:
        if self.is_leaf:
            if not self.is_finish:
                # unsuccessful synthesis, very low artificial label
                return -3

            metrics = self.subtree_leafs_metrics.iloc[0]
            normalized_metrics = (metrics - metrics_distrib.mean()) / metrics_distrib.std()
            return normalized_metrics.dot(METRICS_WEIGHTS)

        subtree_labels = self.get_subtree_leafs_labels(metrics_distrib)
        return LAMBDA * subtree_labels.max() + (1 - LAMBDA) * subtree_labels.mean()

    @cached_property
    def alternative_siblings(self) -> dict:
        bindings, refactorings, dataflows = 0, 0, 0

        if self.parent:
            for sibling in self.parent.children:
                if sibling.sid == self.sid:
                    continue
                if sibling.decision.tag == "BindDecisionView":
                    bindings += 1
                elif sibling.decision.tag == "DataflowDecisionView":
                    dataflows += 1
                else:
                    refactorings += 1

        return dict(alt_bindings=bindings,
                    alt_refactorings=refactorings,
                    alt_dataflows=dataflows)

    async def retrieve_subforest(self, session, levels_left=None):
        self.children = []
        if self.is_leaf or levels_left == -1:
            return

        async with session.get(NITTA_BASEURL + f"/node/{self.sid}/subForest") as resp:
            children_raw = await resp.json()

        log_debug(f"{len(children_raw)} children from {self.sid}")

        for child_raw in children_raw:
            child = NittaNode.from_dict(child_raw)
            child.parent = self
            self.children.append(child)

        levels_left_for_child = None if levels_left is None else levels_left - 1
        await asyncio.gather(
            *[child.retrieve_subforest(session, levels_left_for_child) for child in self.children]
        )


async def retrieve_whole_nitta_tree(max_depth=None) -> NittaNode:
    start_time = time.perf_counter()
    async with ClientSession() as session:
        async with session.get(NITTA_BASEURL + f"/node/-") as resp:
            root_raw = await resp.json()
        root = NittaNode.from_dict(root_raw)
        await root.retrieve_subforest(session, max_depth)

    print(f"Finished tree retrieval in {time.perf_counter() - start_time:.2f} s")
    return root


def _extract_params_dict(node: NittaNode) -> dict:
    if node.decision.tag in ["BindDecisionView", "DataflowDecisionView"]:
        result = node.parameters.copy()
        if node.decision.tag == "DataflowDecisionView":
            result["pNotTransferableInputs"] = sum(result["pNotTransferableInputs"])
        return result
    elif node.decision.tag == "RootView":
        return {}
    else:
        # refactorings
        return {"pRefactoringType": node.decision.tag}


def assemble_tree_dataframe(example: str, node: NittaNode, metrics_distrib=None, include_label=True,
                            levels_left=None) -> pd.DataFrame:
    if include_label and metrics_distrib is None:
        metrics_distrib = node.subtree_leafs_metrics

    self_df = pd.DataFrame(dict(
        example=example,
        sid=node.sid,
        tag=node.decision.tag,
        old_score=node.score,
        is_leaf=node.is_leaf,
        **node.alternative_siblings,
        **_extract_params_dict(node),
    ), index=[0])
    if include_label:
        self_df["label"] = node.compute_label(metrics_distrib)

    levels_left_for_child = None if levels_left is None else levels_left - 1
    if node.is_leaf or levels_left == -1:
        return self_df
    else:
        result = [assemble_tree_dataframe(example, child, metrics_distrib, include_label, levels_left_for_child)
                  for child in node.children]
        if node.sid != "-":
            result.insert(0, self_df)
        return pd.concat(result)


async def process_example(example: str, nitta_root_dir: str = r"../..",
                          nitta_exe_path: str = "stack exec nitta -- ") -> pd.DataFrame:
    if is_port_in_use(NITTA_PORT):
        raise RuntimeError(f"Port {NITTA_PORT} is already in use, shutdown NITTA server if that's running.")

    example_name = os.path.basename(example)

    print(f"Processing example {example!r}")
    with subprocess.Popen(f"{nitta_exe_path} -p={NITTA_PORT} {example}", cwd=nitta_root_dir,
                          stdout=subprocess.PIPE, stderr=subprocess.STDOUT, shell=True) as proc:
        try:
            print(f"NITTA is running.")
            time.sleep(WAIT_NITTA_DELAY)
            print(f"Retrieving tree...")

            tree = await retrieve_whole_nitta_tree()
            with open(DATA_ROOT / f"{example_name}.pickle", "wb") as f:
                pickle.dump(tree, f)

            print(f"Nodes: {tree.subtree_size}. Building dataframe...")
            df = assemble_tree_dataframe(example_name, tree).reset_index(drop=True)

            print(f"Data's ready, {len(df)} rows")

            target_filepath = DATA_ROOT / f"{example_name}.csv"
            print(f"Saving to {target_filepath}")
            df.to_csv(target_filepath, index=False)
        finally:
            proc.kill()
            print(f"NITTA is dead")
    print("DONE")
    return df
