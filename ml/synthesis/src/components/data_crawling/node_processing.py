from __future__ import annotations

from cachetools import cached, Cache

from components.data_crawling.nitta_node import NittaNode


def _extract_params_dict(node: NittaNode) -> dict:
    if node.decision.tag in ["BindDecisionView", "DataflowDecisionView"]:
        result = node.parameters.copy()
        if node.decision.tag == "DataflowDecisionView":
            result["pNotTransferableInputs"] = sum(result["pNotTransferableInputs"])
        return result
    elif node.decision.tag == "RootView":
        return {}
    else:
        # refactorings, which have arbitrary decision tags
        return {"pRefactoringType": node.decision.tag}


def _extract_alternative_siblings_dict(node: NittaNode, siblings: tuple[NittaNode]) -> dict:
    bindings, refactorings, dataflows = 0, 0, 0

    for sibling in siblings:
        if sibling.sid == node.sid:
            continue
        if sibling.decision.tag == "BindDecisionView":
            bindings += 1
        elif sibling.decision.tag == "DataflowDecisionView":
            dataflows += 1
        else:
            # refactorings have arbitrary decision tags
            refactorings += 1

    return dict(alt_bindings=bindings,
                alt_refactorings=refactorings,
                alt_dataflows=dataflows)


@cached(cache=Cache(10000))
def nitta_node_to_df_dict(node: NittaNode, siblings: tuple[NittaNode], example: str = None, ) -> dict:
    return dict(
        example=example,
        sid=node.sid,
        tag=node.decision.tag,
        old_score=node.score,
        is_terminal=node.is_terminal,
        **_extract_alternative_siblings_dict(node, siblings),
        **_extract_params_dict(node),
    )
