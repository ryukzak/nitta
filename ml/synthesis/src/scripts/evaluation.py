import asyncio
import sys
from pathlib import Path
from components.data_crawling.example_running import run_nitta
from components.data_crawling.tree_retrieving import retrieve_whole_nitta_tree
from components.data_crawling.tree_retrieving import retrieve_subforest
from components.data_processing.feature_engineering import preprocess_df
from components.data_processing.feature_engineering import df_to_model_columns
from components.data_crawling.tree_processing import assemble_tree_dataframe
from components.data_crawling.nitta_node import NittaNode
from components.common.logging import get_logger, configure_logging
from mlbackend.models_store import models

from consts import DATA_DIR, ROOT_DIR, MODELS_DIR
import time
import pandas as pd
import tensorflow as tf
from aiohttp import ClientSession
from collections import defaultdict
from aiohttp import ServerDisconnectedError
from IPython.display import display
from time import perf_counter
import argparse

model, model_metainfo = models["example_model_1"]


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

    children = node.children

    if children_limit:
        children = children[:children_limit]

    children = [(evaluator(child), child) for child in node.children]
    children.sort(key=lambda v: v[0], reverse=True)
    #     print(f"children: {[d[0] for d in children]}")

    while children:
        next_best_child = children.pop(0)[1]
        #         print(f"next best: {next_best_child}")
        result = await select_best_by_evaluator(session, evaluator, next_best_child, nitta_baseurl, counters,
                                                children_limit)
        if result is not None:
            return result

    return None


def old_evaluator(node: NittaNode):
    return node.score



def new_evaluator(node: NittaNode):
    final_columns = ['alt_bindings', 'alt_refactorings', 'alt_dataflows', 'pOutputNumber', 'pAlternative',
                     'pAllowDataFlow', 'pCritical', 'pPercentOfBindedInputs', 'pPossibleDeadlock',
                     'pNumberOfBindedFunctions', 'pRestless', 'pNotTransferableInputs', 'pRestrictedTime', 'pWaitTime',
                     'tag_BindDecisionView', 'tag_BreakLoopView', 'tag_ConstantFoldingView', 'tag_DataflowDecisionView',
                     'tag_OptimizeAccumView', 'tag_ResolveDeadlockView']
    metrics_columns = [cn for cn in final_columns if cn.startswith("p")] + ["pRefactoringType", "pWave"]

    node_df = assemble_tree_dataframe("", node, include_label=False, levels_left=-1)
    filled_metrics_df = pd.concat([pd.DataFrame(columns=metrics_columns), node_df])
    preprocessed_df = preprocess_df(filled_metrics_df)

    final_df = df_to_model_columns(preprocessed_df, model_columns=final_columns)

    return model.predict(final_df.values)[0][0]


def reset_counters():
    global counters
    counters = defaultdict(lambda: 0)
    
def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("example_path", type=str, help="Path to the example file")
    return parser.parse_args()

async def main(args):
    example = Path(args.example_path)

    logger = get_logger(__name__)
    configure_logging()
    start_time = perf_counter()

    reset_counters()

    logger.info(f"Selected algorithm: {example}")

    nitta_tree = None
    async with run_nitta(example) as (proc, nitta_baseurl):
        try:
            nitta_tree = await retrieve_whole_nitta_tree(nitta_baseurl)
            new_evaluator(nitta_tree.children[0])
            reset_counters()
            root = await retrieve_whole_nitta_tree(nitta_baseurl)

            async with ClientSession() as session:
                best_new = await select_best_by_evaluator(session, new_evaluator, root, nitta_baseurl, counters, 2)
                logger.info("NEW DONE %s", best_new)
                best_old = await select_best_by_evaluator(session, old_evaluator, root, nitta_baseurl, counters, 2)
                logger.info("OLD DONE %s", best_old)

        finally:
            proc.kill()

    display(best_old)
    display(best_new)
    display(pd.DataFrame(dict(duration=[best_old.duration, best_new.duration],
                              depth=[best_old.depth, best_new.depth],
                              evaluator_calls=[counters["old_evaluator"], counters["new_evaluator"]]),
                         index=["old", "new"]))

    logger.info(f"Finished in {perf_counter() - start_time:.2f} s")

async def test_script():
    class Args:
        def __init__(self, example_path):
            self.example_path = example_path

    args = Args("examples/fibonacci.lua")
    await main(args)    

if __name__ == "__main__":
    args = parse_args()
    asyncio.run(main(args))
# Раскомментируйте следующую строку для выполнения тестовой функции
  # asyncio.run(test_script())