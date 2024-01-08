from pathlib import Path
from tempfile import TemporaryDirectory

import numpy as np
import pandas as pd

from components.common.data_loading import load_all_existing_training_data
from components.common.saving import save_df_with_timestamp
from components.data_processing.dataset_creation import TARGET_COLUMNS


def test_only_correct_data_csvs_are_loaded_from_data_dir():
    with TemporaryDirectory() as tmp_data_dir_name:
        test_cols = [f"col_{i}" for i in range(10)] + TARGET_COLUMNS
        test_training_data_rows = 30
        test_training_data = pd.DataFrame(np.zeros((test_training_data_rows, len(test_cols))), columns=test_cols)
        save_df_with_timestamp(test_training_data, tmp_data_dir_name, "test_data", "test training data")
        save_df_with_timestamp(test_training_data, tmp_data_dir_name, "test_data_2", "other test training data")

        non_training_data = pd.DataFrame(np.zeros((15, 4)), columns=[f"col_{i}" for i in range(4)])
        save_df_with_timestamp(non_training_data, tmp_data_dir_name, "other_data", "non-training data")

        loaded_data = load_all_existing_training_data(Path(tmp_data_dir_name))
        assert len(loaded_data) == test_training_data_rows * 2
