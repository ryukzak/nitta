from dataclasses import dataclass

from dataclasses_json import dataclass_json


@dataclass
class ModelMetainfo:
    train_mae: float
    validation_mae: float


# workaround for PyCharm's understanding that it's @dataclass
ModelMetainfo = dataclass_json(ModelMetainfo)
