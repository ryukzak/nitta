from __future__ import annotations

from typing import cast

import pandas as pd
from sklearn.model_selection import train_test_split
from tensorflow.data import Dataset  # pyright: ignore[reportMissingModuleSource]

from components.common.logging import get_logger

logger = get_logger(__name__)

TARGET_COLUMNS = ["label"]

_DEFAULT_VAL_DS_SPLIT = 0.2
_MAX_DEFAULT_VAL_DS_SIZE = 100_000


def _df_to_dataset(df, shuffle=True, batch_size=16, repeat=False):
    df = df.copy()

    # split df into features and labels
    targets = df[TARGET_COLUMNS].copy()
    df = df.drop(TARGET_COLUMNS, axis=1)
    inputs = df
    input_cols = inputs.columns.tolist()

    ds = Dataset.from_tensor_slices((inputs.to_numpy(), targets.to_numpy()))
    ds = ds.shuffle(buffer_size=10000) if shuffle else ds
    ds = ds.batch(batch_size) if batch_size else ds
    ds = ds.repeat() if repeat else ds
    return ds, input_cols


def create_datasets(df: pd.DataFrame, val_df: pd.DataFrame | None = None) -> tuple[Dataset, Dataset, list[str]]:
    # create training and evaluation datasets
    if val_df is not None:
        # making mypy happy with manual casts, pandas' typings are suboptimal here?
        # (why DataFrame.sample() -> FrameOrSeries? should be the same as caller as stated in the docs?)
        train_df = cast(pd.DataFrame, df.sample(frac=1))
        val_df = cast(pd.DataFrame, val_df.sample(frac=1))
    else:
        # If we have lots of training data, a fixed split (20%, for ex.) for val_ds results in a too big len(val_ds).
        # It slows down the training, as after every epoch the model should be evaluated on a full val_ds.
        # Thus, we limit the split so that _MAX_DEFAULT_VAL_DS_SIZE is not exceeded.
        val_ds_split = min(_DEFAULT_VAL_DS_SPLIT, _MAX_DEFAULT_VAL_DS_SIZE / len(df))
        train_df, val_df = train_test_split(df.sample(frac=1), test_size=val_ds_split)

    n = len(df)
    logger.info(f"N:\t{n}")
    logger.info(f"Train:\t{len(train_df)}, {len(train_df) / n * 100:.0f}%")
    logger.info(f"Val:\t{len(val_df)}, {len(val_df) / n * 100:.0f}%")

    train_ds, train_input_cols = _df_to_dataset(train_df, batch_size=128, repeat=True)
    val_ds, val_input_cols = _df_to_dataset(val_df)

    assert train_input_cols == val_input_cols, "train and val input columns should be interchangeable"
    logger.info(f"Input columns: {train_input_cols}")

    return train_ds, val_ds, train_input_cols
