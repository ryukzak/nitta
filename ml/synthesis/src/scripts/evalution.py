import asyncio
import sys
from pathlib import Path
from components.data_crawling.example_running import run_nitta
from components.data_crawling.tree_retrieving import retrieve_whole_nitta_tree
from components.data_crawling.tree_retrieving import retrieve_subforest
from components.data_processing.feature_engineering import preprocess_df
from components.data_processing.feature_engineering import df_to_model_columns
from components.data_processing.feature_engineering import assemble_tree_dataframe
from components.data_crawling.nitta_node import NittaNode
from components.data_crawling.tree_operations import select_best_by_evaluator
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
WAIT_NITTA_DELAY = 0.5

model, model_metainfo = models["example_model_1"]

def old_evaluator(node: NittaNode):
    return node.score

def new_evaluator(node: NittaNode):
    final_columns = ['alt_bindings', 'alt_refactorings', 'alt_dataflows', 'pOutputNumber', 'pAlternative', 'pAllowDataFlow', 'pCritical', 'pPercentOfBindedInputs', 'pPossibleDeadlock', 'pNumberOfBindedFunctions', 'pRestless', 'pNotTransferableInputs', 'pRestrictedTime', 'pWaitTime', 'tag_BindDecisionView', 'tag_BreakLoopView', 'tag_ConstantFoldingView', 'tag_DataflowDecisionView', 'tag_OptimizeAccumView', 'tag_ResolveDeadlockView']
    metrics_columns = [cn for cn in final_columns if cn.startswith("p")] + ["pRefactoringType", "pWave"]
    
    node_df = assemble_tree_dataframe("", node, include_label=False, levels_left=-1)
    filled_metrics_df = pd.concat([pd.DataFrame(columns=metrics_columns), node_df])
    preprocessed_df = preprocess_df(filled_metrics_df)
    
    final_df = df_to_model_columns(preprocessed_df, model_columns=final_columns)
    
    return model.predict(final_df.values)[0][0]

def reset_counters():
    global counters
    counters = defaultdict(lambda: 0)  


async def main():
    logger = get_logger(__name__)
    configure_logging()
    start_time = perf_counter()
    
    reset_counters()
    
    example = Path(r"examples/fibonacci.lua")
    if len(sys.argv) == 2:
        example = Path(sys.argv[1])
    logger.info(f"Selected algorithm: {example}")
    
    nitta_tree = None
    async with run_nitta(example) as (proc, nitta_baseurl):
        try:
            time.sleep(WAIT_NITTA_DELAY)
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
                              evaluator_calls=[counters["old_evaluator"], counters["new_evaluator"]]), index=["old", "new"]))
    
    logger.info(f"Finished in {perf_counter() - start_time:.2f} s")
    
if __name__ == "__main__":
    asyncio.run(main())