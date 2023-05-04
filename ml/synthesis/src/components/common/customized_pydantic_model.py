from pydantic import BaseModel

from components.utils.string import snake_to_lower_camel_case


class CustomizedBaseModel(BaseModel):
    class Config(BaseModel.Config):
        allow_population_by_field_name = True
        alias_generator = snake_to_lower_camel_case
