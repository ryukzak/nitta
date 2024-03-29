from __future__ import annotations

import csv
from pathlib import Path

import pandas as pd
from pandas import DataFrame

from components.common.logging import get_logger
from components.data_processing.dataset_creation import TARGET_COLUMNS
from consts import DATA_DIR

logger = get_logger(__name__)

_target_columns_set = set(TARGET_COLUMNS)


def load_all_existing_training_data(data_dir: Path = DATA_DIR) -> DataFrame:
    all_csvs = list(data_dir.glob("*.csv"))
    logger.info("Loading all existing training data.")
    logger.info(f"Found a total of {len(all_csvs)} CSVs in {data_dir}, checking for training data...")

    found_csvs: list[Path] = []
    for csv_path in all_csvs:
        with csv_path.open("r") as f:
            if _target_columns_set.issubset(set(next(csv.reader(f)))):
                found_csvs.append(csv_path)

    if not found_csvs:
        raise FileNotFoundError("No training data found. Launch data crawling before training.")

    logger.info(f"Found valid training data in {len(found_csvs)} CSVs, loading...")
    return pd.concat([pd.read_csv(d) for d in found_csvs]).reset_index(drop=True)
