from __future__ import annotations

import pandas as pd
from pandas import DataFrame

from components.common.logging import get_logger

logger = get_logger(__name__)


def _map_bool(c):
    return c.apply(lambda v: 1 if v is True else (0 if v is False else v))


def _map_categorical(df, c):
    return pd.concat([df.drop([c.name], axis=1), pd.get_dummies(c, prefix=c.name)], axis=1)


def preprocess_input_data_df(df_orig: DataFrame) -> DataFrame:
    df: DataFrame = df_orig.copy()

    for bool_column in [
        "is_terminal",
        "pCritical",
        "pPossibleDeadlock",
        "pRestrictedTime",
    ]:
        if bool_column in df.columns:
            df[bool_column] = _map_bool(df[bool_column])
        else:
            logger.warning(f"Column/parameter {bool_column} not found in provided node info.")

    df = _map_categorical(df, df.tag)
    df = df.drop(
        # drop columns that must be excluded at all times
        ["pWave", "example", "sid", "old_score", "is_terminal", "pRefactoringType"],
        axis="columns",
        errors="ignore",
    )
    df = df.fillna(0)  # fill NaNs in columns (mostly OHE flags) with 0
    return df


def df_to_model_columns(df: DataFrame, input_cols: list[str]) -> DataFrame:
    df = pd.concat([pd.DataFrame(columns=input_cols), df])[input_cols]  # reset columns, fill data if possible
    df = df.fillna(0)  # fill NaNs that might've just appeared with 0s
    return df
