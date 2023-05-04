from components.data_crawling.nitta_node import NittaNode
from components.data_crawling.node_processing import nitta_node_to_df_dict


def test_node_to_raw_dict(test_tree, test_tree_dict):
    assert NittaNode.to_dict(test_tree) == test_tree_dict


def test_node_from_raw_dict(test_tree, test_tree_dict):
    assert NittaNode.from_dict(test_tree_dict) == test_tree


def test_node_to_train_df_dict(test_tree):
    assert nitta_node_to_df_dict(test_tree, siblings=tuple(test_tree.children), example="test") == {
        "alt_bindings": 1,
        "alt_dataflows": 0,
        "alt_refactorings": 0,
        "example": "test",
        "is_terminal": False,
        "old_score": 1010,
        "sid": "-",
        "tag": "RootView",
    }
