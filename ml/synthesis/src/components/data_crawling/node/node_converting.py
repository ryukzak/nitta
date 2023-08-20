from __future__ import annotations

from typing import Iterable

from components.common.nitta_node import NittaNode


def nitta_node_to_df_dict(
    node: NittaNode,
    siblings: Iterable[NittaNode],
    example: str | None = None,
) -> dict:
    return dict(
        example=example,
        sid=node.sid,
        tag=node.decision_tag,
        old_score=node.score,
        is_terminal=node.is_terminal,
        **_extract_alternative_siblings_dict(node, siblings),
        **_extract_params_dict(node),
    )


def _extract_params_dict(node: NittaNode) -> dict:
    if node.decision_tag in ["BindDecisionView", "DataflowDecisionView"]:
        assert isinstance(node.parameters, dict), "parameters must be a dict for Bind and Dataflow decisions"
        result = node.parameters.copy()
        if node.decision_tag == "DataflowDecisionView":
            result["pNotTransferableInputs"] = sum(result["pNotTransferableInputs"])
        return result

    if node.decision_tag == "RootView":
        return {}

    # only refactorings left, which have arbitrary decision tags
    return {"pRefactoringType": node.decision_tag}


def _extract_alternative_siblings_dict(node: NittaNode, siblings: Iterable[NittaNode]) -> dict:
    bindings, refactorings, dataflows = 0, 0, 0

    for sibling in siblings:
        if sibling.sid == node.sid:
            continue
        if sibling.decision_tag == "BindDecisionView":
            bindings += 1
        elif sibling.decision_tag == "DataflowDecisionView":
            dataflows += 1
        else:
            # refactorings have arbitrary decision tags
            refactorings += 1

    return dict(alt_bindings=bindings, alt_refactorings=refactorings, alt_dataflows=dataflows)
