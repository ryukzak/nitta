import argparse
import asyncio
import os
import signal
import sys
from asyncio import sleep
from collections import defaultdict, deque
from contextlib import asynccontextmanager
from dataclasses import dataclass, field
from pathlib import Path
from time import perf_counter
from typing import Any, AsyncGenerator, Deque, List, Optional, Tuple

import numpy as np
import pandas as pd
from aiohttp import ClientSession, ServerDisconnectedError
from cached_property import cached_property
from cachetools import cached
from dataclasses_json import LetterCase, dataclass_json

_METRICS_WEIGHTS = pd.Series(dict(duration=-1, depth=-0.1))
_LAMBDA = 0.6


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

    children: Optional[List["NittaNode"]] = field(default=None, repr=False)
    parent: Optional["NittaNode"] = field(default=None, repr=False)

    def __hash__(self):
        return hash(self.sid)

    @property
    def is_leaf(self):
        return self.is_terminal

    @cached_property
    def subtree_size(self):
        assert self.children is not None
        return sum(child.subtree_size for child in self.children) + 1

    @cached_property
    def depth(self) -> int:
        return self.sid.count("-") if self.sid != "-" else 0

    @cached_property
    def subtree_leafs_metrics(self) -> Optional[Deque[Tuple[int, int]]]:
        """:returns: deque(tuple(duration, depth)) or None if node is a failed leaf"""
        if self.is_leaf:
            if not self.is_finish:
                return None
            return deque(((self.duration, self.depth),))
        else:
            children_metrics = (
                child.subtree_leafs_metrics for child in self.children if child.subtree_leafs_metrics is not None
            )
            return sum(children_metrics, deque())

    @cached_node_method
    def get_subtree_leafs_labels(self, metrics_distrib: np.ndarray) -> deque:
        if self.is_leaf:
            return deque((self.compute_label(metrics_distrib),))
        else:
            return sum(
                (child.get_subtree_leafs_labels(metrics_distrib) for child in self.children),
                deque(),
            )

    @cached_node_method
    def compute_label(self, metrics_distrib: np.ndarray) -> float:
        if self.is_leaf:
            if not self.is_finish:
                # unsuccessful synthesis, very low artificial label
                return -3

            # (duration, depth)
            metrics = np.array(self.subtree_leafs_metrics[0])

            # if std is 0, then we have a single value getting normalized. the nominator is also zero.
            # let's define normalized_metrics for this edge case as all-zeros, so they don't break anything.
            # adding an epsilon to avoid division by zero.
            normalized_metrics = (metrics - metrics_distrib.mean(axis=0)) / (metrics_distrib.std(axis=0) + 1e-5)

            return normalized_metrics.dot(_METRICS_WEIGHTS)

        subtree_labels = np.array(self.get_subtree_leafs_labels(metrics_distrib))
        return _LAMBDA * subtree_labels.max() + (1 - _LAMBDA) * subtree_labels.mean()


async def retrieve_subforest(
    node: NittaNode,
    session: ClientSession,
    nitta_baseurl: str,
    levels_left=None,
):
    node.children = []
    if node.is_leaf or levels_left == -1:
        return

    async with session.get(f"{nitta_baseurl}/node/{node.sid}/subForest") as resp:
        children_raw = await resp.json()

    for child_raw in children_raw:
        child = NittaNode.from_dict(child_raw)
        child.parent = node
        node.children.append(child)

    levels_left_for_child = None if levels_left is None else levels_left - 1
    await asyncio.gather(
        *[retrieve_subforest(child, session, nitta_baseurl, levels_left_for_child) for child in node.children]
    )


async def retrieve_whole_nitta_tree(nitta_baseurl: str, max_depth=None) -> NittaNode:
    async with ClientSession() as session:
        async with session.get(nitta_baseurl + "/node/-") as resp:
            root_raw = await resp.json()
        root = NittaNode.from_dict(root_raw)
        await retrieve_subforest(root, session, nitta_baseurl, max_depth)

    return root


@asynccontextmanager
async def run_nitta(
    example: Path,
    nitta_exe_path: str = "stack exec nitta -- ",
    nitta_args: str = "",
    nitta_env: dict = None,
    port: int = 8090,
) -> AsyncGenerator[Tuple[asyncio.subprocess.Process, str], None]:
    nitta_baseurl = f"http://localhost:{port}"

    cmd = f"{nitta_exe_path} -p={port} {nitta_args} {example}"
    print(cmd)
    env = os.environ.copy()
    env.update(nitta_env or {})

    proc = None
    try:
        preexec_fn = None if os.name == "nt" else os.setsid  # see https://stackoverflow.com/a/4791612
        proc = await asyncio.create_subprocess_shell(
            cmd,
            cwd=str("."),
            stdout=sys.stdout,
            stderr=sys.stderr,
            shell=True,
            preexec_fn=preexec_fn,
            env=env,
        )

        await sleep(2)

        yield proc, nitta_baseurl
    finally:
        if proc is not None and proc.returncode is None:
            pid = proc.pid
            pgid = os.getpgid(pid)
            os.killpg(pgid, signal.SIGTERM)
            await proc.wait()


async def select_best_by_evaluator(session, evaluator, node, nitta_baseurl, counters, children_limit=None):
    counters[evaluator.__name__] += 1

    if node.is_leaf:
        if not node.is_finish:
            return None

        return node

    try:
        await retrieve_subforest(node, session, nitta_baseurl)
    except ServerDisconnectedError:
        return None

    children = node.children

    if children_limit:
        children = children[:children_limit]

    children = [(evaluator(child), child) for child in node.children]
    children.sort(key=lambda v: v[0], reverse=True)

    while children:
        next_best_child = children.pop(0)[1]
        result = await select_best_by_evaluator(
            session,
            evaluator,
            next_best_child,
            nitta_baseurl,
            counters,
            children_limit,
        )
        if result is not None:
            return result

    return None


def old_evaluator(node: NittaNode):
    return node.score


def reset_counters():
    global counters
    counters = defaultdict(lambda: 0)


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("example_paths", type=str, nargs="+", help="Paths to the example files")
    parser.add_argument(
        "--nitta_args",
        type=str,
        default="",
        help="Additional arguments for Nitta",
    )
    return parser.parse_args()


async def main(args):
    examples = args.example_paths

    df_list = []  # List to hold all dataframes

    for example in examples:
        example_path = Path(example)

        reset_counters()

        print(f"Selected algorithm: {example_path.stem}")

        async with run_nitta(example_path, nitta_args=args.nitta_args) as (
            proc,
            nitta_baseurl,
        ):
            root = await retrieve_whole_nitta_tree(nitta_baseurl)

            async with ClientSession() as session:
                result_dict = {}
                evaluator = old_evaluator
                start_time = perf_counter()
                best = await select_best_by_evaluator(session, evaluator, root, nitta_baseurl, counters, 2)
                end_time = perf_counter() - start_time
                result_dict = {
                    "duration": best.duration,
                    "depth": best.depth,
                    "evaluator_calls": counters[evaluator.__name__],
                    "time": end_time,
                }
                print(f"{example_path.stem.upper()} DONE {best}")
                print(f"Finished {example_path.stem} in {end_time:.2f} s")

                # Create a DataFrame for each example
                df = pd.DataFrame(result_dict, index=[example_path.stem])
                df_list.append(df)  # Append the df to the list

    # Concatenate all the DataFrames in the list
    results_df = pd.concat(df_list)
    print(results_df)


if __name__ == "__main__":
    args = parse_args()
    asyncio.run(main(args))
