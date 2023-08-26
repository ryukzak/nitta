from components.common.logging import silence_unwanted_logs
from tests.fixtures.test_tree_root import test_root, test_root_dict

# imported fixtures from other modules
# noinspection PyStatementEffect
(
    test_root,
    test_root_dict,
)


def pytest_sessionstart(session):
    silence_unwanted_logs()
