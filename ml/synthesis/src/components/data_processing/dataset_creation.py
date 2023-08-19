from typing import List, Optional, Tuple, cast

import pandas as pd
from sklearn.model_selection import train_test_split
from tensorflow.data import Dataset  # pyright: ignore[reportMissingModuleSource]

from components.common.logging import get_logger

logger = get_logger(__name__)

TARGET_COLUMNS = ["label"]


def _df_to_dataset(df, shuffle=True, batch_size=16, repeat=False):
    df = df.copy()

    # split df into features and labels
    targets = df[TARGET_COLUMNS].copy()
    df.drop(TARGET_COLUMNS, axis=1, inplace=True)
    inputs = df
    input_cols = inputs.columns.values.tolist()

    ds = Dataset.from_tensor_slices((inputs.values, targets.values))
    ds = ds.shuffle(buffer_size=10000) if shuffle else ds
    ds = ds.batch(batch_size) if batch_size else ds
    ds = ds.repeat() if repeat else ds
    return ds, input_cols


def create_datasets(df: pd.DataFrame, val_df: Optional[pd.DataFrame] = None) -> Tuple[Dataset, Dataset, List[str]]:
    # create training and evaluation datasets
    if val_df is not None:
        # making mypy happy with manual casts, pandas' typings are suboptimal here?
        # (why DataFrame.sample() -> FrameOrSeries? should be the same as caller as stated in the docs?)
        train_df = cast(pd.DataFrame, df.sample(frac=1))
        val_df = cast(pd.DataFrame, val_df.sample(frac=1))
    else:
        # TODO: if we have lots of training data, len(val_ds) gets large and slows down the training,
        #  as the same val_ds should be evaluated fully after each epoch
        train_df, val_df = train_test_split(df.sample(frac=1), test_size=0.2)

    n = len(df)
    logger.info(f"N:\t{n}")
    logger.info(f"Train:\t{len(train_df)}, {len(train_df) / n * 100:.0f}%")
    logger.info(f"Val:\t{len(val_df)}, {len(val_df) / n * 100:.0f}%")

    train_ds, train_input_cols = _df_to_dataset(train_df, batch_size=128, repeat=True)
    val_ds, val_input_cols = _df_to_dataset(val_df)

    assert train_input_cols == val_input_cols, "train and val input columns should be interchangeable"
    logger.info(f"Input columns: {train_input_cols}")

    return train_ds, val_ds, train_input_cols
