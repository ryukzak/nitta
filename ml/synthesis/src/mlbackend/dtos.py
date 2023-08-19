from typing import Generic, List, TypeVar

from pydantic import Field
from pydantic.generics import GenericModel

from components.common.customized_pydantic_model import CustomizedBaseModel
from components.common.nitta_node import NittaNode

TData = TypeVar("TData")


class Response(GenericModel, Generic[TData]):
    data: TData


class ModelInfo(CustomizedBaseModel):
    name: str = Field(
        description="Name of the model",
        example="production",
    )
    train_mae: float = Field(
        description="Mean absolute error on the training set",
        example=0.12,
    )
    validation_mae: float = Field(
        description="Mean absolute error on the validation set",
        example=0.178,
    )


class ScoringInput(CustomizedBaseModel):
    """
    Single input for score prediction model.

    This data structure is designed to be model-agnostic (i.e. it should contain enough information for any model).
    This lets us avoid branching data-gathering code on Haskell side based on model which it's currently talking to (in
    other words, moving model input formatting responsibility from Haskell code to ML backend code). So, NITTA is able
    not to care about which model it talks to and even transparently and dynamically switch them if user wants it.

    This design choice is relevant when we have more than one ML model. It's expected to be so in the future. Some
    models may be experimental, some better work for one type of things, some for another. Seems like a nice feature
    to have.

    This approach has a downside: possible inefficiency. Some gathered data may be not used by some models. It's chosen
    to ignore that for now since optimizations will definitely be possible when they become needed. For example, we can
    always add mentioned data gathering branching based on currently used model at the cost of reduced flexibility.
    """

    scoring_target: str = Field(
        description="SID of a node which we need to predict the score for. This node must be in `nodes` list. "
        "You can also pass the value `all` to get scores for all nodes in the `nodes` list.",
        example="-0-4-7-3-4-1-1-0",
    )
    nodes: List[NittaNode] = Field(
        description="`NodeView`s of scoring target node and all its siblings (all possible synthesis tree choices "
        "from current parent node).",
    )
    # parents?
    # decision history? (can differ from parents!)


class PostScoreRequestBody(CustomizedBaseModel):
    inputs: List[ScoringInput] = Field(description="List of inputs to get score predictions for. ")


# data, not whole body (which can include "data" field and metadata)
class PostScoreResponseData(CustomizedBaseModel):
    scores: List[List[float]] = Field(
        description="List lists of numeric score predictions. A list per each input in the request body.",
        example=[[0.1, 0.2], [0.4314]],
    )
