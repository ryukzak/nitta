import pytest

from components.data_crawling.nitta_node import NittaNode, NittaNodeDecision


# TODO: fix parent issue

@pytest.fixture()
def test_tree():
    return NittaNode(
        score=1010,
        is_terminal=False,
        is_finish=False,
        parameters={"a": 1, "b": 2},
        decision=NittaNodeDecision(tag="RootView"),
        duration=None,
        sid="-",
        parent=None,
        children=[
            NittaNode(
                score=1020,
                is_terminal=True,
                is_finish=True,
                parameters={"a": 1, "b": 2},
                decision=NittaNodeDecision(tag="BindDecisionView"),
                duration=None,
                sid="-0",
                children=[],
            )
        ],
    )


@pytest.fixture()
def test_tree_dict():
    return {
        "score": 1010,
        "isTerminal": False,
        "isFinish": False,
        "parameters": {"a": 1, "b": 2},
        "decision": {"tag": "RootView"},
        "duration": None,
        "sid": "-",
        "parent": None,
        "children": [
            {
                "score": 1020,
                "isTerminal": True,
                "isFinish": True,
                "parameters": {"a": 1, "b": 2},
                "decision": {"tag": "BindDecisionView"},
                "duration": None,
                "sid": "-0",
                "parent": None,
                "children": [],
            },
        ],
    }
