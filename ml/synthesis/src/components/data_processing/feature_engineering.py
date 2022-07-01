import pandas as pd

from pandas import DataFrame


def _map_bool(c):
    return c.apply(lambda v: 1 if v is True else (0 if v is False else v))


def _map_categorical(df, c):
    return pd.concat([df.drop([c.name], axis=1), pd.get_dummies(c, prefix=c.name)], axis=1)


def preprocess_df(df: DataFrame) -> DataFrame:
    df: DataFrame = df.copy()
    df.is_leaf = _map_bool(df.is_leaf)
    df.pCritical = _map_bool(df.pCritical)
    df.pPossibleDeadlock = _map_bool(df.pPossibleDeadlock)
    df.pRestrictedTime = _map_bool(df.pRestrictedTime)
    df = _map_categorical(df, df.tag)
    df = df.drop(["pWave", "example", "sid", "old_score", "is_leaf", "pRefactoringType"], axis="columns")
    df = df.fillna(0)
    return df
