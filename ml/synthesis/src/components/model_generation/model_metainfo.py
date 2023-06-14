from typing import List, Optional

from components.common.customized_pydantic_model import CustomizedBaseModel


class ModelMetainfo(CustomizedBaseModel):
    train_mae: float
    validation_mae: float
    input_columns: Optional[List[str]] = None
