from collections import deque
from typing import Deque, Optional

import numpy as np
import pandas as pd
from joblib import Parallel, delayed

from components.data_crawling.nitta_node import NittaNode


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
                            levels_left=None, n_workers: int = 1) -> pd.DataFrame:
    if include_label and metrics_distrib is None:
        metrics_distrib = np.array(node.subtree_leafs_metrics)

    def child_process_job(node: NittaNode):
        accum = deque()
        _assemble_tree_dataframe_recursion(accum, example, node, metrics_distrib, include_label, levels_left)
        return accum

    if n_workers > 1:
        deques = Parallel(n_jobs=n_workers)(delayed(child_process_job)(node) for node in node.children)
    else:
        deques = [child_process_job(node)]

    return pd.DataFrame(sum(deques, deque()))


def _assemble_tree_dataframe_recursion(accum: Deque[dict], example: str, node: NittaNode, metrics_distrib: np.ndarray,
                                       include_label: bool, levels_left: Optional[int]):
    self_dict = dict(
        example=example,
        sid=node.sid,
        tag=node.decision.tag,
        old_score=node.score,
        is_leaf=node.is_leaf,
        **node.alternative_siblings,
        **_extract_params_dict(node),
    )
    if include_label:
        self_dict["label"] = node.compute_label(metrics_distrib)

    if node.is_leaf or levels_left == -1:
        accum.append(self_dict)
    else:
        levels_left_for_child = None if levels_left is None else levels_left - 1
        for child in node.children:
            _assemble_tree_dataframe_recursion(accum, example, child, metrics_distrib, include_label,
                                               levels_left_for_child)
            if node.sid != "-":
                accum.appendleft(self_dict)  # so it's from roots to leaves
