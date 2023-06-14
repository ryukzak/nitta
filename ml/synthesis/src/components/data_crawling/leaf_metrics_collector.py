from typing import Dict, Union

import numpy as np

from components.common.logging import get_logger
from components.common.nitta_node import NittaNode
from components.data_crawling.node_processing import get_leaf_metrics

logger = get_logger(__name__)


class SingleMetricCollector:
    """
    Collects values from a series to store info about them and calculate their mean and stddev:
    - iteratively (without storing all values)
    - lazily (only when requested)

    See https://math.stackexchange.com/questions/2148877/iterative-calculation-of-mean-and-standard-deviation
    """

    def __init__(self, name):
        self.name = name
        self.n = 0  # number of values seen so far
        self.s1 = 0.0  # sum of values (see link above)
        self.s2 = 0.0  # sum of squared values (see link above)

    def collect(self, value: Union[float, int]):
        self.n += 1
        self.s1 += value
        self.s2 += value * value

    def compute_mean(self) -> float:
        return self.s1 / self.n

    def compute_stddev(self) -> float:
        return ((self.s2 - self.s1 * self.s1 / self.n) / self.n) ** 0.5

    def __str__(self):
        return (
            f"{type(self).__name__}({self.name}: n={self.n}, mean={self.compute_mean()}, "
            f"stddev={self.compute_stddev()}))"
        )


class LeafMetrics:
    DURATION = "duration"
    DEPTH = "depth"

    @classmethod
    def all(cls):
        return (cls.DURATION, cls.DEPTH)


class LeafMetricsCollector:
    """Collects leaf nodes, stores their metrics and computes metrics distributions when needed"""

    def __init__(self):
        self.collectors: Dict[str, SingleMetricCollector] = {
            metric: SingleMetricCollector(metric) for metric in LeafMetrics.all()
        }
        self._cached_distributions = None

    def collect_leaf_node(self, node: NittaNode, ignore_unsuccessful: bool = True):
        assert (
            node.is_terminal
        ), "only leaf nodes should be collected for metrics distributions"

        if not node.is_finish:
            assert (
                ignore_unsuccessful
            ), "shouldn't collect metrics from unsuccessful synthesis leafs"
            return

        duration, depth = get_leaf_metrics(node)

        if duration is not None:
            self.collectors[LeafMetrics.DURATION].collect(duration)
        else:
            logger.warning(
                f"Tried to collect metrics for node without duration: {node}"
            )

        self.collectors[LeafMetrics.DEPTH].collect(depth)

        self._cached_distributions = None

    def get_distributions(self) -> np.ndarray:
        """
        :return: array [ [means...], [stddevs...] ], where means and stddevs are for (duration, depth)
        """
        if self._cached_distributions is None:
            self._cached_distributions = np.array(
                [
                    [c.compute_mean() for c in self.collectors.values()],
                    [c.compute_stddev() for c in self.collectors.values()],
                ]
            )
        return self._cached_distributions
