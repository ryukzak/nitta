from __future__ import annotations

from typing import List, Optional

from components.common.logging import get_logger
from components.common.nitta_node import NittaNodeInTree
from components.data_crawling.leaf_metrics_collector import (
    LeafMetrics,
    LeafMetricsCollector,
)
from components.data_crawling.node_label_computation import compute_node_label
from components.data_crawling.node_processing import nitta_node_to_df_dict

logger = get_logger(__name__)


# We need to decay the node label while backpropagating to bias the model (and synthesis) towards the leafs.
# The easiest way to do it is to multiply the label by some decay coefficient on each (child -> parent) backpropagation
# step. The bias will be exponential - that is desired.
#
# Since synthesis process will continuously take nodes with max label, the decay coefficient will control breadth-first
# vs depth-first tree search behaviour:
#   - larger decay coefficient -> weaker decay -> more depth-first search;
#   - smaller decay coefficient -> stronger decay -> more breadth-first search.
#
# We also need to account for the fact that trees can be of different depth (5, 500, even 5000?). That's why the final
# decay coefficient is computed based on the average leaf depth in the tree (to be precise, its on-the-fly estimate).
#
# Hence, more tree-agnostic way to control the decay rate is to specify the desired part of leaf label that should end
# up in the root node's label. Here's an example, assuming f=0.9 - decay coefficient; d=3 - leaf depth:
#       (root)    -->    (node1)     -->    (node2)     -->    (leaf)
#       d=0              d=1                d=2                d=3
#       label=0.729      label=0.81         label=0.9          label=1
#       label=f^3 * L    label=f^2 * L      label=f * L        label=L
#
#       any node label  = f^d * L
#       root label      = f^d * L = R - part of leaf label that should end up in the root.
#
#       f is computed dynamically based on known d and R, assuming L = 1: f = R^(1/d) (d-th root of R)
#       R is the parameter that's controlled below.
#           1 - no decay,
#           0.01 - very strong decay (to near-zero),
#           0 - infinite decay (to zero right after the first step).

_PART_OF_LEAF_LABEL_IN_ROOT_LABEL = 0.5


def assemble_training_data_via_backpropagation_from_leaf(
    results_accum: List[dict],
    leaf: NittaNodeInTree,
    metrics_collector: LeafMetricsCollector,
    example_name: Optional[str] = None,
):
    label = compute_node_label(leaf, metrics_collector)

    depth_collector = metrics_collector.collectors[LeafMetrics.DEPTH]
    if depth_collector.n > 0:
        current_mean_leaf_depth = depth_collector.compute_mean()
        label_decay_coefficient = _PART_OF_LEAF_LABEL_IN_ROOT_LABEL ** (1 / current_mean_leaf_depth)
    else:
        # we haven't met a single successful leaf yet, so won't decay anyway
        assert label < 0, "expected only labels for unsuccessful leafs (<0) here"
        label_decay_coefficient = 1

    node = leaf
    while node.parent is not None:
        if node.parent.children is None:
            logger.warning(f"Parent of {node.sid} has None children! Concurrent modification?")
        siblings = node.parent.children or []

        node_dict = nitta_node_to_df_dict(node, siblings, example_name)
        node_dict["label"] = label
        results_accum.append(node_dict)

        # don't decay negative labels for unsuccessful nodes
        if label > 0:
            label *= label_decay_coefficient

        node = node.parent
