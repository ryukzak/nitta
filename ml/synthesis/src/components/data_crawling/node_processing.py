from __future__ import annotations

from collections import deque
from typing import Optional, Deque, Tuple

from components.data_crawling.nitta_node import NittaNode
from components.utils.cache import cached


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


class UnknownSubtreeSize(RuntimeError):
    pass


@cached()
def get_subtree_size(node: NittaNode) -> int:
    if node.children is None:
        raise UnknownSubtreeSize()

    result = 0

    for child in node.children:
        child_size = get_subtree_size(child)
        if child_size is None:
            raise UnknownSubtreeSize()
        result += child_size

    return result + 1


@cached()
def get_depth(node: NittaNode) -> int:
    return node.sid.count('-') if node.sid != '-' else 0

# def subtree_leafs_metrics(node: NittaNode) -> Optional[Deque[Tuple[int, int]]]:
#     """ :returns: deque(tuple(duration, depth)) or None if node is a failed leaf """
#     if node.is_terminal:
#         if not node.is_finish:
#             return None
#         return deque(((node.duration, get_depth(node)),))
#     else:
#         children_metrics = \
#             (child.subtree_leafs_metrics for child in node.children if child.subtree_leafs_metrics is not None)
#         return sum(children_metrics, deque())
