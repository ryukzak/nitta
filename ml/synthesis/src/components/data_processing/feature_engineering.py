from __future__ import annotations
import pandas as pd

from pandas import DataFrame

from components.common.logging import get_logger

logger = get_logger(__name__)


def _map_bool(c):
    return c.apply(lambda v: 1 if v is True else (0 if v is False else v))


def _map_categorical(df, c):
    return pd.concat([df.drop([c.name], axis=1), pd.get_dummies(c, prefix=c.name)], axis=1)


def preprocess_df(df: DataFrame) -> DataFrame:
    df: DataFrame = df.copy()

    for bool_column in ["is_leaf", "pCritical", "pPossibleDeadlock", "pRestrictedTime"]:
        if bool_column in df.columns:
            df[bool_column] = _map_bool(df[bool_column])
        else:
            logger.warning(f"Column/parameter {bool_column} not found in provided node info.")

    df = _map_categorical(df, df.tag)
    df = df.drop(["pWave", "example", "sid", "old_score", "is_leaf", "pRefactoringType"], axis="columns", errors="ignore")
    df = df.fillna(0)
    return df


# TODO: move that to metainfo of the model, find a way to make input building model-dependent
#  (pickled module? function name?)
_BASELINE_MODEL_COLUMNS = \
    ["alt_bindings", "alt_refactorings", "alt_dataflows", "pAllowDataFlow", "pAlternative", "pCritical",
     "pNumberOfBindedFunctions", "pOutputNumber", "pPercentOfBindedInputs", "pPossibleDeadlock", "pRestless",
     "pFirstWaveOfTargetUse", "pNotTransferableInputs", "pRestrictedTime", "pWaitTime", "tag_BindDecisionView",
     "tag_BreakLoopView", "tag_DataflowDecisionView"]


def df_to_model_columns(df: DataFrame, model_columns: list[str] = None) -> DataFrame:
    if not model_columns:
        model_columns = _BASELINE_MODEL_COLUMNS

    df = pd.concat([pd.DataFrame(columns=model_columns), df])[model_columns]  # reset columns, fill data if possible
    df = df.fillna(0)  # fill NaNs in columns (mostly OHE flags) with 0
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

def assemble_tree_dataframe(example: str, node: NittaNode, metrics_distrib=None, include_label=True,
                            levels_left=None) -> pd.DataFrame:
    if include_label and metrics_distrib is None:
        metrics_distrib = node.subtree_leafs_metrics

    self_df = pd.DataFrame(dict(
        example=example,
        sid=node.sid,
        tag=node.decision.tag,
        old_score=node.score,
        is_leaf=node.is_leaf,
        **_extract_params_dict(node),
    ), index=[0])
    if include_label:
        self_df["label"] = node.compute_label(metrics_distrib)

    levels_left_for_child = None if levels_left is None else levels_left - 1
    if node.is_leaf or levels_left == -1:
        return self_df
    else:
        result = [assemble_tree_dataframe(example, child, metrics_distrib, include_label, levels_left_for_child)
                  for child in node.children]
        if node.sid != "-":
            result.insert(0, self_df)
        return pd.concat(result)
