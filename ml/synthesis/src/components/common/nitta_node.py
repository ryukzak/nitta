from typing import List, Optional, Union

from pydantic import Field

from components.common.customized_pydantic_model import CustomizedBaseModel


# related to corresponding NITTA REST API DTO
# TODO: link those types to NITTA Haskell source code via code generation like it's done with with TypeScript?
class NittaNode(CustomizedBaseModel):
    """`NodeView` from NITTA Haskell sources."""

    sid: str = Field(example="-0-4-7-3-4-1-1-0")
    score: Optional[int]
    is_terminal: bool
    is_finish: bool
    duration: Optional[int]
    # union with str because {parameters: "root"} is currently possible
    parameters: Union[dict, str] = Field(
        description="SynthesisDecision.metrics from NITTA Haskell sources.",
        example={
            "pAllowDataFlow": 0,
            "pPossibleDeadlock": False,
            "pPercentOfBindedInputs": 0,
            "pRestless": 0,
            "pNumberOfBindedFunctions": 0,
            "pWave": 4,
            "pOutputNumber": 1,
            "pAlternative": 2,
            "pCritical": False,
        },
    )
    decision: dict = Field(
        description="`SynthesisDecision.decision` from NITTA Haskell sources.",
        example={
            "tag": "BindDecisionView",
            "function": {"fvFun": "loop(0.000000, res^0#0) = i^0#0", "fvHistory": []},
            "pu": "fram1",
        },
    )

    @property
    def decision_tag(self) -> Optional[str]:
        return self.decision.get("tag")

    def __hash__(self):
        return hash(self.sid)


class NittaNodeInTree(NittaNode):
    children: Optional[List["NittaNodeInTree"]] = Field(default=None, repr=False, exclude=True)
    parent: Optional["NittaNodeInTree"] = Field(default=None, repr=False, exclude=True)

    @property
    def is_loaded(self) -> bool:
        return self.children is not None

    @staticmethod
    def from_node(node: NittaNode) -> "NittaNodeInTree":
        """
        Helps putting a non-tree node info in a tree context (i.e. adding tree-related attributes to the node object)
        """
        return NittaNodeInTree(**node.dict())  # is it the most efficient way?

    def dict(self, **overrides):
        """Customized Pydantic's .dict() to provide convenient defaults for NittaNodeInTree"""
        kwargs: dict = dict(by_alias=True)
        kwargs.update(overrides)
        return super().dict(**kwargs)
