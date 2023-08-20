from __future__ import annotations

from typing import TypeVar

import numpy as np
import orjson
from aiohttp import ClientSession

from components.common.customized_pydantic_model import CustomizedBaseModel
from components.common.logging import get_logger
from components.common.nitta_node import NittaNode, NittaNodeInTree

logger = get_logger(__name__)


# there used to be a method + dto to retrieve a /treeInfo, but it's wasn't used, so it got removed
# https://github.com/ryukzak/nitta/blob/ff1fa9228489f83d90e7f5ca89151347f42b5d48/ml/synthesis/src/components/data_crawling/tree_retrieving.py#L88


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

    node.children = []

    for child_raw in children_raw:
        child = NittaNodeInTree.parse_obj(child_raw)
        child.parent = node
        node.children.append(child)


TResponse = TypeVar("TResponse", bound=CustomizedBaseModel)


async def _do_nitta_request(
    nitta_baseurl: str,
    session: ClientSession,
    path: str,
    response_type: type[TResponse],
    method: str = "GET",
) -> TResponse:
    async with session.request(method, nitta_baseurl + path) as resp:
        raw = await resp.json(loads=orjson.loads)
        # is this a good idea? not too much logging? remove if so
        logger.debug(f"Parsed a response from NITTA: {method} {path} -> {raw}")
    return response_type.parse_obj(raw)


async def retrieve_single_node(sid: str, nitta_baseurl: str, session: ClientSession) -> NittaNode:
    return await _do_nitta_request(nitta_baseurl, session, f"/node/{sid}", NittaNode)


async def retrieve_tree_root(nitta_baseurl: str, session: ClientSession) -> NittaNodeInTree:
    node = await retrieve_single_node("-", nitta_baseurl, session)
    tree_root = NittaNodeInTree.from_node(node)
    return tree_root


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
