from components.common.customized_pydantic_model import CustomizedBaseModel


class ModelMetainfo(CustomizedBaseModel):
    train_mae: float
    validation_mae: float
