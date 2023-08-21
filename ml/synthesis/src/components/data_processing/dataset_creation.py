from __future__ import annotations

from typing import cast

import pandas as pd
from sklearn.model_selection import train_test_split
from tensorflow.data import Dataset  # pyright: ignore[reportMissingModuleSource]

from components.common.logging import get_logger
from components.data_processing.feature_engineering import df_to_model_columns

logger = get_logger(__name__)

TARGET_COLUMNS = ["label"]

_DEFAULT_VAL_DS_SPLIT = 0.2
_MAX_DEFAULT_VAL_DS_SIZE = 100_000


def create_datasets(main_df: pd.DataFrame, val_df: pd.DataFrame | None = None) -> tuple[Dataset, Dataset, list[str]]:
    # create training and evaluation datasets
    if val_df is not None:
        n = len(main_df) + len(val_df)
        # making mypy happy with manual casts, pandas' typings are suboptimal here?
        # (why DataFrame.sample() -> FrameOrSeries? should be the same as caller as stated in the docs?)
        train_df = cast(pd.DataFrame, main_df.sample(frac=1))
        val_df = cast(pd.DataFrame, val_df.sample(frac=1))
    else:
        # If we have lots of training data, a fixed split (20%, for ex.) for val_ds results in a too big len(val_ds).
        # It slows down the training, as after every epoch the model should be evaluated on a full val_ds.
        # Thus, we limit the split so that _MAX_DEFAULT_VAL_DS_SIZE is not exceeded.
        n = len(main_df)
        val_ds_split = min(_DEFAULT_VAL_DS_SPLIT, _MAX_DEFAULT_VAL_DS_SIZE / len(main_df))
        train_df, val_df = train_test_split(main_df.sample(frac=1), test_size=val_ds_split)

    logger.info(f"N:\t{n}")
    logger.info(f"Train:\t{len(train_df)}, {len(train_df) / n * 100:.0f}%")
    logger.info(f"Val:\t{len(val_df)}, {len(val_df) / n * 100:.0f}%")

    train_df_inputs, train_df_targets = _split_df_into_targets_and_inputs(train_df)
    val_df_inputs, val_df_targets = _split_df_into_targets_and_inputs(val_df)
    del train_df, val_df

    train_df_inputs, val_df_inputs = _ensure_train_and_val_df_have_same_columns(train_df_inputs, val_df_inputs)
    input_columns = train_df_inputs.columns.tolist()
    logger.info(f"Input columns: {input_columns}")

    train_ds = _df_to_dataset(train_df_inputs, train_df_targets, batch_size=128, repeat=True)
    val_ds = _df_to_dataset(val_df_inputs, val_df_targets)
    del train_df_inputs, val_df_inputs

    return train_ds, val_ds, input_columns


def _split_df_into_targets_and_inputs(df: pd.DataFrame) -> tuple[pd.DataFrame, pd.DataFrame]:
    targets = df[TARGET_COLUMNS]
    inputs = df.drop(TARGET_COLUMNS, axis=1)
    return inputs, targets


def _df_to_dataset(df: pd.DataFrame, targets: pd.DataFrame, shuffle=True, batch_size=16, repeat=False):
    ds = Dataset.from_tensor_slices((df.to_numpy(), targets.to_numpy()))
    ds = ds.shuffle(buffer_size=10000) if shuffle else ds
    ds = ds.batch(batch_size) if batch_size else ds
    ds = ds.repeat() if repeat else ds
    return ds


def _ensure_train_and_val_df_have_same_columns(
    train_df: pd.DataFrame,
    val_df: pd.DataFrame,
) -> tuple[pd.DataFrame, pd.DataFrame]:
    if train_df.columns.tolist() == val_df.columns.tolist():
        return train_df, val_df

    logger.warning("Detected a train/val input columns mismatch. Fixing it...")

    train_cols = set(train_df.columns.tolist())
    val_cols = set(val_df.columns.tolist())
    in_train_not_in_val = train_cols - val_cols
    in_val_not_in_train = val_cols - train_cols
    if in_train_not_in_val:
        logger.info(f"Columns in train_df but not in val_df: {in_train_not_in_val}")
    if in_val_not_in_train:
        logger.info(f"Columns in val_df but not in train_df: {in_val_not_in_train}")

    final_cols = list(sorted(train_cols | val_cols))

    logger.info("Rebuilding train/val dataframes...")
    train_df = df_to_model_columns(train_df, final_cols)
    val_df = df_to_model_columns(val_df, final_cols)

    return train_df, val_df
