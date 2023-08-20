from components.common.nitta_node import NittaNodeInTree
from components.data_crawling.node.node_converting import nitta_node_to_df_dict


def test_node_to_raw_dict(test_root: NittaNodeInTree, test_root_dict):
    assert test_root.dict(by_alias=True) == test_root_dict


def test_node_from_raw_dict(test_root, test_root_dict):
    assert NittaNodeInTree.parse_obj(test_root_dict) == test_root


def test_node_to_train_df_dict(test_root: NittaNodeInTree):
    assert test_root.children, "test root must have children for this test to run"
    assert nitta_node_to_df_dict(test_root, siblings=test_root.children, example="test") == {
        "alt_bindings": 1,
        "alt_dataflows": 0,
        "alt_refactorings": 0,
        "example": "test",
        "is_terminal": False,
        "old_score": 1010,
        "sid": "-",
        "tag": "RootView",
    }
