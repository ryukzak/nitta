from typing import Tuple

from sklearn.model_selection import train_test_split
from tensorflow.python.data import Dataset

from components.common.logging import get_logger

logger = get_logger(__name__)

TARGET_COLUMNS = ["label"]


def _df_to_dataset(df, shuffle=True, batch_size=16, repeat=False, log_columns=False):
    df = df.copy()

    # split df into features and labels
    targets = df[TARGET_COLUMNS].copy()
    df.drop(TARGET_COLUMNS, axis=1, inplace=True)
    features = df
    if log_columns:
        logger.info(f"Feature columns: {features.columns.values.tolist()}")

    ds = Dataset.from_tensor_slices((features.values, targets.values))
    ds = ds.shuffle(buffer_size=10000) if shuffle else ds
    ds = ds.batch(batch_size) if batch_size else ds
    ds = ds.repeat() if repeat else ds
    return ds


def create_datasets(df) -> Tuple[Dataset, Dataset]:
    # create training and evaluation datasets
    train_df, test_df = train_test_split(df.sample(frac=1), test_size=0.2)

    n = len(df)
    logger.info(f"N:\t{n}")
    logger.info(f"Train:\t{len(train_df)}, {len(train_df) / n * 100:.0f}%")
    logger.info(f"Test:\t{len(test_df)}, {len(test_df) / n * 100:.0f}%")

    train_ds = _df_to_dataset(train_df, batch_size=16, repeat=True, log_columns=True)
    test_ds = _df_to_dataset(test_df)

    return train_ds, test_ds
