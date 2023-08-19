import asyncio
import time
from typing import Type, TypeVar

import numpy as np
import orjson
from aiohttp import ClientSession

from components.common.customized_pydantic_model import CustomizedBaseModel
from components.common.logging import get_logger
from components.common.nitta_node import NittaNode, NittaNodeInTree, NittaTreeInfo
from components.common.utils import debounce

logger = get_logger(__name__)
logger_debug_debounced = debounce(1)(logger.debug)


async def retrieve_children(
    node: NittaNodeInTree,
    session: ClientSession,
    nitta_baseurl: str,
    ignore_loaded: bool = False,
):
    if node.is_loaded:
        if not ignore_loaded:
            raise ValueError(f"Attempted to load children of a node twice: {node}")
        return

    async with session.get(f"{nitta_baseurl}/node/{node.sid}/subForest") as resp:
        children_raw = await resp.json(loads=orjson.loads)

    logger_debug_debounced(f"{len(children_raw)} children from {node.sid}")

    node.children = []

    for child_raw in children_raw:
        child = NittaNodeInTree.parse_obj(child_raw)
        child.parent = node
        node.children.append(child)


async def retrieve_subforest(
    node: NittaNodeInTree,
    session: ClientSession,
    nitta_baseurl: str,
    levels_left=None,
):
    if node.is_terminal or levels_left == -1:
        node.children = []
        return

    await retrieve_children(node, session, nitta_baseurl)
    assert node.children is not None, "children should be loaded by now"

    levels_left_for_child = None if levels_left is None else levels_left - 1
    await asyncio.gather(
        *[retrieve_subforest(child, session, nitta_baseurl, levels_left_for_child) for child in node.children]
    )


TResponse = TypeVar("TResponse", bound=CustomizedBaseModel)


async def _do_nitta_request(
    nitta_baseurl: str,
    session: ClientSession,
    path: str,
    response_type: Type[TResponse],
    method: str = "GET",
) -> TResponse:
    async with session.request(method, nitta_baseurl + path) as resp:
        raw = await resp.json(loads=orjson.loads)
        # is this a good idea? not too much logging? remove if so
        logger.debug(f"Parsed a response from NITTA: {method} {path} -> {raw}")
    return response_type.parse_obj(raw)


async def retrieve_tree_root(nitta_baseurl: str, session: ClientSession) -> NittaNodeInTree:
    node = await _do_nitta_request(nitta_baseurl, session, "/node/-", NittaNode)
    tree_root = NittaNodeInTree.from_node(node)
    return tree_root


async def retrieve_tree_info(nitta_baseurl: str, session: ClientSession) -> NittaTreeInfo:
    return await _do_nitta_request(nitta_baseurl, session, "/treeInfo", NittaTreeInfo)


async def retrieve_whole_nitta_tree(nitta_baseurl: str, max_depth=None) -> NittaNodeInTree:
    start_time = time.perf_counter()
    async with ClientSession() as session:
        tree = await retrieve_tree_root(nitta_baseurl, session)
        await retrieve_subforest(tree, session, nitta_baseurl, max_depth)

    logger.info(f"Finished tree retrieval in {time.perf_counter() - start_time:.2f} s")
    return tree


async def retrieve_random_descending_thread(
    root: NittaNodeInTree,
    nitta_baseurl: str,
    session: ClientSession,
    ignore_dirty_tree: bool = True,
) -> NittaNodeInTree:
    """
    :returns: a leaf that we've randomly descended to
    """
    node = root

    while not node.is_terminal:
        if not node.is_loaded:
            await retrieve_children(node, session, nitta_baseurl, ignore_loaded=ignore_dirty_tree)
        elif not ignore_dirty_tree:
            raise ValueError(f"Node was expected to be not loaded: {node}")
        assert node.children is not None, "children should be loaded by now"

        if len(node.children) == 0:
            logger.warning(f"Non-terminal node {node.sid} has no children, stopping descent")
            break

        # weight descending probability by score
        # this is a quick way to increase representation of successful nodes in results of the big trees
        shift = 3000  # to avoid negative scores
        pivot = shift + 2000
        weights = np.array([child.score for child in node.children])
        weights = ((weights + shift) / pivot) ** 3

        node = np.random.choice(
            # 1st arg's typing (ArrayLike) doesn't accept pure python lists, but in runtime it's fine
            node.children,  # type: ignore
            p=weights / weights.sum(),
        )

    return node


async def retrieve_single_node(nitta_baseurl: str, sid: str) -> NittaNode:
    # TODO: unify with _do_nitta_request and retrieve_tree_root
    async with ClientSession() as session:
        async with session.get(f"{nitta_baseurl}/node/{sid}") as resp:
            node_raw = await resp.json(loads=orjson.loads)
    return NittaNode.parse_obj(node_raw)
