from __future__ import annotations

import pandas as pd
from pandas import DataFrame

from components.common.logging import get_logger

logger = get_logger(__name__)


def _map_bool(c):
    # TODO: 1 - 0 - -1 mapping
    return c.apply(lambda v: 1 if v is True else (0 if v is False else v))


def _map_categorical(df, c):
    return pd.concat([df.drop([c.name], axis=1), pd.get_dummies(c, prefix=c.name)], axis=1)


def preprocess_input_data_df(input_df: DataFrame) -> DataFrame:
    df: DataFrame = input_df.copy()
    df = df.drop(
        # drop columns that must be excluded at all times
        ["pWave", "example", "sid", "old_score", "is_terminal", "pRefactoringType"],
        axis="columns",
        errors="ignore",
    )
    dtypes_dict = df.convert_dtypes().dtypes.to_dict()

    for col, dtype in dtypes_dict.items():
        if dtype.name == "string":
            df = _map_categorical(df, df[col])
        elif dtype.name == "boolean":
            df[col] = _map_bool(df[col])

    df = df.fillna(0)  # fill NaNs in columns (mostly OHE flags) with 0
    return df


def df_to_model_columns(df: DataFrame, input_cols: list[str]) -> DataFrame:
    df = pd.concat([pd.DataFrame(columns=input_cols), df])[input_cols]  # reset columns, fill data if possible
    df = df.fillna(0)  # fill NaNs that might've just appeared with 0s
    return df
