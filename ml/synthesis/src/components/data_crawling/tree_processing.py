from __future__ import annotations

from collections import deque
from typing import Deque, Optional

import numpy as np
import pandas as pd
from joblib import Parallel, delayed

from components.common.nitta_node import NittaNodeInTree
from components.data_crawling.node_label_computation import compute_node_label
from components.data_crawling.node_processing import nitta_node_to_df_dict


def assemble_tree_dataframe(
    example: str,
    node: NittaNodeInTree,
    metrics_distrib=None,
    include_label=True,
    levels_left=None,
    n_workers: int = 1,
) -> pd.DataFrame:
    if include_label and metrics_distrib is None:
        metrics_distrib = np.array(node.subtree_leafs_metrics)

    def child_process_job(node: NittaNodeInTree):
        accum = deque()
        _assemble_tree_dataframe_recursion(
            accum, example, node, metrics_distrib, include_label, levels_left
        )
        return accum

    if n_workers > 1:
        deques = Parallel(n_jobs=n_workers)(
            delayed(child_process_job)(node) for node in node.children
        )
    else:
        deques = [child_process_job(node)]

    return pd.DataFrame(sum(deques, deque()))


def _assemble_tree_dataframe_recursion(
    accum: Deque[dict],
    example: str,
    node: NittaNodeInTree,
    metrics_distrib: np.ndarray,
    include_label: bool,
    levels_left: Optional[int],
):
    siblings = (node.parent.children or []) if node.parent else []
    self_dict = nitta_node_to_df_dict(node, tuple(siblings), example)

    if include_label:
        self_dict["label"] = compute_node_label(node, metrics_distrib)

    if node.is_terminal or levels_left == -1:
        accum.append(self_dict)
    else:
        levels_left_for_child = None if levels_left is None else levels_left - 1
        for child in node.children:
            _assemble_tree_dataframe_recursion(
                accum,
                example,
                child,
                metrics_distrib,
                include_label,
                levels_left_for_child,
            )
            if node.sid != "-":
                accum.appendleft(self_dict)  # so it's from roots to leaves
