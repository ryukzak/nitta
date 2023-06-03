import argparse
import asyncio
from collections import defaultdict
from pathlib import Path
from time import perf_counter

import pandas as pd
import tensorflow as tf
from aiohttp import ClientSession, ServerDisconnectedError
from components.common.logging import configure_logging, get_logger
from components.data_crawling.example_running import run_nitta
from components.data_crawling.nitta_node import NittaNode
from components.data_crawling.tree_retrieving import retrieve_subforest, retrieve_whole_nitta_tree
from components.data_processing.feature_engineering import df_to_model_columns
from consts import MODELS_DIR
from IPython.display import display

model = tf.keras.models.load_model(MODELS_DIR)


def preprocess_df(df: pd.DataFrame) -> pd.DataFrame:
    def map_bool(c):
        return c.apply(lambda v: 1 if v is True else (0 if v is False else v))

    def map_categorical(df, c, options=None):
        return pd.concat(
            [
                df.drop([c.name], axis=1),
                pd.get_dummies(c, prefix=c.name, columns=options),
            ],
            axis=1,
        )

    df = df.copy()
    df.is_leaf = map_bool(df.is_leaf)
    df.pCritical = map_bool(df.pCritical)
    df.pPossibleDeadlock = map_bool(df.pPossibleDeadlock)
    df.pRestrictedTime = map_bool(df.pRestrictedTime)
    df = map_categorical(
        df,
        df.tag,
        [
            "tag_BindDecisionView",
            "tag_BreakLoopView",
            "tag_ConstantFoldingView",
            "tag_DataflowDecisionView",
            "tag_OptimizeAccumView",
            "tag_ResolveDeadlockView",
        ],
    )
    df = df.drop(
        [
            "pWave",
            "example",
            "sid",
            "old_score",
            "is_leaf",
            "pRefactoringType",
        ],
        axis="columns",
    )

    df = df.fillna(0)
    return df


def _extract_params_dict(node: NittaNode) -> dict:
    if node.decision.tag in ["BindDecisionView", "DataflowDecisionView"]:
        result = node.parameters.copy()
        if node.decision.tag == "DataflowDecisionView":
            result["pNotTransferableInputs"] = sum(result["pNotTransferableInputs"])
        return result
    elif node.decision.tag == "RootView":
        return {}
    else:
        # refactorings
        return {"pRefactoringType": node.decision.tag}


def assemble_tree_dataframe(
    example: str,
    node: NittaNode,
    metrics_distrib=None,
    include_label=True,
    levels_left=None,
) -> pd.DataFrame:
    if include_label and metrics_distrib is None:
        metrics_distrib = node.subtree_leafs_metrics

    self_df = pd.DataFrame(
        dict(
            example=example,
            sid=node.sid,
            tag=node.decision.tag,
            old_score=node.score,
            is_leaf=node.is_leaf,
            **_extract_params_dict(node),
        ),
        index=[0],
    )
    if include_label:
        self_df["label"] = node.compute_label(metrics_distrib)

    levels_left_for_child = None if levels_left is None else levels_left - 1
    if node.is_leaf or levels_left == -1:
        return self_df
    else:
        result = [
            assemble_tree_dataframe(
                example,
                child,
                metrics_distrib,
                include_label,
                levels_left_for_child,
            )
            for child in node.children
        ]
        if node.sid != "-":
            result.insert(0, self_df)
        return pd.concat(result)


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
        result = await select_best_by_evaluator(
            session,
            evaluator,
            next_best_child,
            nitta_baseurl,
            counters,
            children_limit,
        )
        if result is not None:
            return result

    return None


def old_evaluator(node: NittaNode):
    return node.score


def new_evaluator(node: NittaNode):
    final_columns = [
        "alt_bindings",
        "alt_refactorings",
        "alt_dataflows",
        "pOutputNumber",
        "pAlternative",
        "pAllowDataFlow",
        "pCritical",
        "pPercentOfBindedInputs",
        "pPossibleDeadlock",
        "pNumberOfBindedFunctions",
        "pRestless",
        "pNotTransferableInputs",
        "pRestrictedTime",
        "pWaitTime",
        "tag_BindDecisionView",
        "tag_BreakLoopView",
        "tag_ConstantFoldingView",
        "tag_DataflowDecisionView",
        "tag_OptimizeAccumView",
        "tag_ResolveDeadlockView",
    ]
    metrics_columns = [cn for cn in final_columns if cn.startswith("p")] + [
        "pRefactoringType",
        "pWave",
    ]

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
    parser.add_argument("example_paths", type=str, nargs="+", help="Paths to the example files")
    parser.add_argument(
        "--evaluator",
        type=str,
        nargs="+",
        choices=["nitta", "ml"],
        help="Evaluator to use",
    )
    parser.add_argument(
        "--nitta_args",
        type=str,
        default="",
        help="Additional arguments for Nitta",
    )
    return parser.parse_args()


async def main(args):
    examples = args.example_paths
    evaluator_choices = args.evaluator

    evaluator_dict = {"nitta": old_evaluator, "ml": new_evaluator}

    evaluator_choices = [choice for choice in evaluator_choices if choice in evaluator_dict]
    if not evaluator_choices:
        print("Invalid evaluator choices. Using the new evaluator as default.")
        evaluator_choices = ["nitta"]

    results = []

    for example in examples:
        example = Path(example)

        logger = get_logger(__name__)
        configure_logging()
        reset_counters()

        logger.info(f"Selected algorithm: {example}")

        nitta_tree = None
        async with run_nitta(example, nitta_args=args.nitta_args) as (
            proc,
            nitta_baseurl,
        ):
            nitta_tree = await retrieve_whole_nitta_tree(nitta_baseurl)
            new_evaluator(nitta_tree.children[0])
            reset_counters()
            root = await retrieve_whole_nitta_tree(nitta_baseurl)

            async with ClientSession() as session:
                result_dict = {"example": example, "evaluators": {}}
                for evaluator_choice in evaluator_choices:
                    evaluator = evaluator_dict[evaluator_choice]
                    start_time = perf_counter()
                    best = await select_best_by_evaluator(session, evaluator, root, nitta_baseurl, counters, 2)
                    end_time = perf_counter() - start_time
                    result_dict["evaluators"][evaluator_choice] = {
                        "best": best,
                        "duration": best.duration,
                        "depth": best.depth,
                        "evaluator_calls": counters[evaluator_choice + "_evaluator"],
                        "time": end_time,
                    }
                    logger.info(f"{evaluator_choice.upper()} DONE %s", best)
                    logger.info(f"Finished {evaluator_choice} in {end_time:.2f} s")
                results.append(result_dict)

    for result in results:
        print(f"\nAlgorithm: {result['example']}")
        dfs = []
        for evaluator, evaluator_result in result["evaluators"].items():
            df = pd.DataFrame(
                dict(
                    duration=[evaluator_result["duration"]],
                    depth=[evaluator_result["depth"]],
                    evaluator_calls=[evaluator_result["evaluator_calls"]],
                    time=[evaluator_result["time"]],
                ),
                index=[evaluator],
            )
            dfs.append(df)
        result_df = pd.concat(dfs)
        display(result_df)


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
