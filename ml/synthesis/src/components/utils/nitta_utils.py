from collections import defaultdict
import pandas as pd
from components.data_crawling.nitta_node import NittaNode

def preprocess_df(df: pd.DataFrame) -> pd.DataFrame:
    def map_bool(c):
        return c.apply(lambda v: 1 if v is True else (0 if v is False else v))
    
    def map_categorical(df, c, options=None):
        return pd.concat([df.drop([c.name], axis=1), pd.get_dummies(c, prefix=c.name, columns=options)], axis=1)
    
    df = df.copy()
    df.is_leaf = map_bool(df.is_leaf)
    df.pCritical = map_bool(df.pCritical)
    df.pPossibleDeadlock = map_bool(df.pPossibleDeadlock)
    df.pRestrictedTime = map_bool(df.pRestrictedTime)
    df = map_categorical(df, df.tag, ['tag_BindDecisionView','tag_BreakLoopView','tag_ConstantFoldingView','tag_DataflowDecisionView','tag_OptimizeAccumView','tag_ResolveDeadlockView'])
    df = df.drop(["pWave", "example", "sid", "old_score", "is_leaf", "pRefactoringType"], axis="columns")
    
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