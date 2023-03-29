from collections import deque
from dataclasses import dataclass, field
from typing import Optional, Any, List, Tuple, Deque

import numpy as np
import pandas as pd
from cached_property import cached_property
from cachetools import cached
from dataclasses_json import LetterCase, dataclass_json

# TODO: move tree processing methods to tree_processing.py, preserve caching
# using deque since it's fast to append/extend to - only thing we need


_METRICS_WEIGHTS = pd.Series(dict(duration=-1, depth=-0.1))
_LAMBDA = 0.6


def cached_node_method(wrapped):
    return cached({}, key=lambda self, *args: hash(self.sid))(wrapped)


nitta_dataclass_params = dict(letter_case=LetterCase.CAMEL)


@dataclass_json(**nitta_dataclass_params)
@dataclass
class NittaNodeDecision:
    tag: str


# TODO: refactor: migrate to Pydantic (see mlbackend), split responsibilities
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
        return self.sid.count('-') if self.sid != '-' else 0

    @cached_property
    def subtree_leafs_metrics(self) -> Optional[Deque[Tuple[int, int]]]:
        """ :returns: deque(tuple(duration, depth)) or None if node is a failed leaf """
        if self.is_leaf:
            if not self.is_finish:
                return None
            return deque(((self.duration, self.depth),))
        else:
            children_metrics = \
                (child.subtree_leafs_metrics for child in self.children if child.subtree_leafs_metrics is not None)
            return sum(children_metrics, deque())

    @cached_node_method
    def get_subtree_leafs_labels(self, metrics_distrib: np.ndarray) -> deque:
        if self.is_leaf:
            return deque((self.compute_label(metrics_distrib),))
        else:
            return sum((child.get_subtree_leafs_labels(metrics_distrib) for child in self.children), deque())

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
