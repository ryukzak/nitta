from pathlib import Path
from tempfile import TemporaryDirectory

import numpy as np

from components.common.data_loading import load_all_existing_training_data
from components.common.model_loading import load_model
from components.common.utils import strip_none_from_tensor_shape
from components.data_crawling.example_running import run_example_and_retrieve_tree_data
from components.data_processing.dataset_creation import create_datasets, TARGET_COLUMNS
from components.data_processing.feature_engineering import preprocess_df
from components.model_generation.training import train_and_save_baseline_model
from consts import EXAMPLES_DIR


async def test_model_generation_pipeline():
    with TemporaryDirectory() as tmp_data_dir_name:
        with TemporaryDirectory() as tmp_model_dir_name:
            tmp_data_dir = Path(tmp_data_dir_name)
            tmp_models_dir = Path(tmp_model_dir_name)

            await run_example_and_retrieve_tree_data(EXAMPLES_DIR / "fibonacci.lua", data_dir=tmp_data_dir)
            df = load_all_existing_training_data(tmp_data_dir)
            assert df[TARGET_COLUMNS].dropna().size > 0, "Labels weren't calculated"

            pdf = preprocess_df(df)
            tds, vds = create_datasets(pdf)

            model_name = "test-model"
            train_and_save_baseline_model(tds, vds, fitting_kwargs=dict(
                epochs=1,
                steps_per_epoch=1,
            ), output_model_name=model_name, models_dir=tmp_models_dir)

            model, metainfo = load_model(tmp_models_dir / model_name)
            inp_shape = strip_none_from_tensor_shape(model.input_shape)
            out_shape = strip_none_from_tensor_shape(model.output_shape)
            assert model.predict(np.zeros(shape=inp_shape).reshape(1, -1))[0].shape == out_shape
            assert isinstance(metainfo.train_mae, float)
            assert isinstance(metainfo.validation_mae, float)
