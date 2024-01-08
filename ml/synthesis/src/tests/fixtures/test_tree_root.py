import pytest

from components.common.nitta_node import NittaNodeInTree


@pytest.fixture()
def test_root():
    return NittaNodeInTree(
        score=1010,
        is_terminal=False,
        is_finish=False,
        parameters={"a": 1, "b": 2},
        decision=dict(tag="RootView"),
        duration=None,
        sid="-",
        parent=None,
        children=[
            NittaNodeInTree(
                score=1020,
                is_terminal=True,
                is_finish=True,
                parameters={"a": 1, "b": 2},
                decision=dict(tag="SingleBindView"),
                duration=None,
                sid="-0",
                children=[],
            ),
        ],
    )


@pytest.fixture()
def test_root_dict():
    return {
        "score": 1010,
        "isTerminal": False,
        "isFinish": False,
        "parameters": {"a": 1, "b": 2},
        "decision": {"tag": "RootView"},
        "duration": None,
        "sid": "-",
    }
