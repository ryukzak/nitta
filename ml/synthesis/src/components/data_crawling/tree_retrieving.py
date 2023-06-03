import asyncio
import time

from aiohttp import ClientSession
from components.common.logging import get_logger
from components.common.utils import debounce
from components.data_crawling.nitta_node import NittaNode

logger = get_logger(__name__)
logger_debug_debounced = debounce(1)(logger.debug)


async def retrieve_subforest(
    node: NittaNode,
    session: ClientSession,
    nitta_baseurl: str,
    levels_left=None,
):
    node.children = []
    if node.is_leaf or levels_left == -1:
        return

    async with session.get(f"{nitta_baseurl}/node/{node.sid}/subForest") as resp:
        children_raw = await resp.json()

    logger_debug_debounced(f"{len(children_raw)} children from {node.sid}")

    for child_raw in children_raw:
        child = NittaNode.from_dict(child_raw)
        child.parent = node
        node.children.append(child)

    levels_left_for_child = None if levels_left is None else levels_left - 1
    await asyncio.gather(
        *[retrieve_subforest(child, session, nitta_baseurl, levels_left_for_child) for child in node.children]
    )


async def retrieve_whole_nitta_tree(nitta_baseurl: str, max_depth=None) -> NittaNode:
    start_time = time.perf_counter()
    async with ClientSession() as session:
        async with session.get(nitta_baseurl + "/node/-") as resp:
            root_raw = await resp.json()
        root = NittaNode.from_dict(root_raw)
        await retrieve_subforest(root, session, nitta_baseurl, max_depth)

    logger.info(f"Finished tree retrieval in {time.perf_counter() - start_time:.2f} s")
    return root
