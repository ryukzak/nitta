from glob import glob
from pathlib import Path
from typing import List

import pandas as pd
from pandas import DataFrame

from components.common.logging import get_logger
from consts import DATA_DIR

logger = get_logger(__name__)


def load_all_existing_training_data(data_dir: Path = DATA_DIR) -> DataFrame:
    data_csvs: List[str] = glob(str(data_dir / "*lua*.csv"))
    logger.info(f"Loading data from {len(data_csvs)} files")
    return pd.concat([pd.read_csv(d) for d in data_csvs]).reset_index(drop=True)
