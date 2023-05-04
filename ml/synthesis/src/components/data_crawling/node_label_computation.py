from collections import deque

import numpy as np
import pandas as pd

from components.data_crawling.nitta_node import NittaNode
from components.utils.cache import cached

_METRICS_WEIGHTS = pd.Series(dict(duration=-1, depth=-0.1))

# lambda == 0 -> label is max of subtree labels
# lambda == 1 -> label is mean of subtree labels
_LAMBDA = 0.6

# very low artificial label
_UNSUCCESSFUL_SYNTHESIS_LEAF_LABEL = -3

# shifting successful labels to avoid negative labels for useful nodes
# + to further separate successful synthesis from unsuccessful
# [-3 .. 3] (with 99.73% certainty) -> [(shift + -3) .. (shift + 3)]
_SUCCESSFUL_SYNTHESIS_LEAF_LABEL_SHIFT = 10


@cached()
def get_subtree_leafs_labels(node: NittaNode, metrics_distrib: np.ndarray) -> deque:
    if node.is_terminal:
        return deque((compute_node_label(node, metrics_distrib),))
    else:
        return sum((get_subtree_leafs_labels(child, metrics_distrib) for child in node.children), deque())


@cached()
def compute_node_label(node: NittaNode, metrics_distrib: np.ndarray) -> float:
    if node.is_terminal:
        if not node.is_finish:
            return _UNSUCCESSFUL_SYNTHESIS_LEAF_LABEL

        # (duration, depth)
        metrics = np.array(node.subtree_leafs_metrics[0])

        # if std is 0, then we have a single value getting normalized. the nominator is also zero.
        # let's define normalized_metrics for this edge case as all-zeros, so they don't break anything.
        # adding an epsilon to avoid division by zero.
        normalized_metrics = (metrics - metrics_distrib.mean(axis=0)) / (metrics_distrib.std(axis=0) + 1e-5)

        return normalized_metrics.dot(_METRICS_WEIGHTS) + _SUCCESSFUL_SYNTHESIS_LEAF_LABEL_SHIFT

    subtree_labels = np.array(node.get_subtree_leafs_labels(metrics_distrib))
    return _LAMBDA * subtree_labels.max() + (1 - _LAMBDA) * subtree_labels.mean()
