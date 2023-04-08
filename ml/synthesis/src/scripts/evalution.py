import asyncio
from pathlib import Path
from components.data_crawling.example_running import run_nitta
from components.data_crawling.tree_retrieving import retrieve_whole_nitta_tree
from components.data_crawling.tree_retrieving import retrieve_subforest
from components.utils.nitta_utils import preprocess_df
from components.utils.nitta_utils import assemble_tree_dataframe
from components.data_crawling.nitta_node import NittaNode
from components.common.tree_operations import select_best_by_evaluator


from consts import DATA_DIR, ROOT_DIR, MODELS_DIR
import time
import pandas as pd
import tensorflow as tf
from aiohttp import ClientSession
from collections import defaultdict
from aiohttp import ServerDisconnectedError
from IPython.display import display
WAIT_NITTA_DELAY = 0.5

model = tf.keras.models.load_model(MODELS_DIR)

def old_evaluator(node: NittaNode):
    return node.score

def new_evaluator(node: NittaNode):
    final_columns = ['alt_bindings', 'alt_refactorings', 'alt_dataflows', 'pOutputNumber', 'pAlternative', 'pAllowDataFlow', 'pCritical', 'pPercentOfBindedInputs', 'pPossibleDeadlock', 'pNumberOfBindedFunctions', 'pRestless', 'pNotTransferableInputs', 'pRestrictedTime', 'pWaitTime', 'tag_BindDecisionView', 'tag_BreakLoopView', 'tag_ConstantFoldingView', 'tag_DataflowDecisionView', 'tag_OptimizeAccumView', 'tag_ResolveDeadlockView']
    metrics_columns = [cn for cn in final_columns if cn.startswith("p")] + ["pRefactoringType", "pWave"]
    
    node_df = assemble_tree_dataframe("", node, include_label=False, levels_left=-1)
    filled_metrics_df = pd.concat([pd.DataFrame(columns=metrics_columns), node_df])
    preprocessed_df = preprocess_df(filled_metrics_df)
    right_final_columns_df = pd.concat([pd.DataFrame(columns=final_columns), preprocessed_df])[final_columns]
    ohe_flags_zero_filled_df = right_final_columns_df.fillna(0)
    final_df = ohe_flags_zero_filled_df
    
    return model.predict(final_df.values)[0][0]

def reset_counters():
    global counters
    counters = defaultdict(lambda: 0)  


async def main():
    reset_counters()
    
    example = Path(r"examples/fibonacci.lua")
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
                print("NEW DONE", best_new)
                best_old = await select_best_by_evaluator(session, old_evaluator, root, nitta_baseurl, counters, 2)
                print("OLD DONE", best_old)
        
        finally:
            proc.kill()
    

    display(best_old)
    display(best_new)
    display(pd.DataFrame(dict(duration=[best_old.duration, best_new.duration],
                              depth=[best_old.depth, best_new.depth],
                              evaluator_calls=[counters["old_evaluator"], counters["new_evaluator"]]), index=["old", "new"]))
if __name__ == "__main__":
    asyncio.run(main())