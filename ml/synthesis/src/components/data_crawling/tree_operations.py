import asyncio
from aiohttp import ClientSession, ServerDisconnectedError

from components.data_crawling.tree_retrieving import retrieve_subforest

async def select_best_by_evaluator(session, evaluator, node, nitta_baseurl, counters, children_limit=None):
    counters[evaluator.__name__] += 1
    
    if node.is_leaf:
        if not node.is_finish:
            return None
            
        return node

    try:
        await retrieve_subforest(node, session, nitta_baseurl)
    except ServerDisconnectedError:
#         print(f"Invalid node with NITTA exception: {node}")
        return None
    
    children = [(evaluator(child), child) for child in node.children]
    children.sort(key=lambda v: v[0], reverse=True)
#     print(f"children: {[d[0] for d in children]}")
    if children_limit:
        children = children[:children_limit]
    
    while children:
        next_best_child = children.pop(0)[1]
#         print(f"next best: {next_best_child}")
        result = await select_best_by_evaluator(session, evaluator, next_best_child, nitta_baseurl, counters, children_limit)
        if result is not None:
            return result         
        
    return None