import numpy as np

from components.common.nitta_node import NittaNode
from components.data_crawling.leaf_metrics_collector import LeafMetricsCollector


def test_collector_calculates_distributions_correctly():
    collector = LeafMetricsCollector()
    nodes = [
        NittaNode(sid="-0", score=1000, is_terminal=True, is_finish=True, duration=1, parameters={}, decision={}),
        NittaNode(sid="-1", score=1000, is_terminal=True, is_finish=True, duration=2, parameters={}, decision={}),
        NittaNode(sid="-2", score=1000, is_terminal=True, is_finish=True, duration=3, parameters={}, decision={}),
    ]
    for n in nodes:
        collector.collect_leaf_node(n)
    assert (collector.get_distributions() - np.array([
        # (duration_mean, depth_mean)
        [2, 1],
        # (duration_stddev, depth_stddev)
        [0.81649658092773, 0],
    ]) < 1e-5).all()
