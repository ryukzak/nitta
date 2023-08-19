from pathlib import Path
from tempfile import TemporaryDirectory

import numpy as np
from aiohttp import ClientSession

from components.common.data_loading import load_all_existing_training_data
from components.common.logging import get_logger
from components.common.model_loading import load_model_with_metainfo
from components.common.utils import strip_none_from_tensor_shape
from components.data_crawling.example_running import (
    produce_data_for_example,
    run_nitta_server,
)
from components.data_crawling.tree_retrieving import (
    retrieve_children,
    retrieve_tree_root,
)
from components.data_processing.dataset_creation import TARGET_COLUMNS, create_datasets
from components.data_processing.feature_engineering import preprocess_train_data_df
from components.model_generation.training import train_and_save_baseline_model
from consts import EXAMPLES_DIR, EnvVarNames

logger = get_logger(__name__)


async def test_smoke():
    with TemporaryDirectory() as tmp_data_dir_name:
        with TemporaryDirectory() as tmp_model_dir_name:
            # -- model generation pipeline --
            tmp_data_dir = Path(tmp_data_dir_name)
            tmp_models_dir = Path(tmp_model_dir_name)

            await produce_data_for_example(EXAMPLES_DIR / "fibonacci.lua", data_dir=tmp_data_dir)
            df = load_all_existing_training_data(tmp_data_dir)
            assert df[TARGET_COLUMNS].dropna().size > 0, "Labels weren't calculated"

            pdf = preprocess_train_data_df(df)
            tds, vds = create_datasets(pdf)

            model_name = "test_model"
            train_and_save_baseline_model(
                tds,
                vds,
                fitting_kwargs=dict(
                    epochs=1,
                    steps_per_epoch=1,
                ),
                output_model_name=model_name,
                models_dir=tmp_models_dir,
            )

            model, metainfo = load_model_with_metainfo(tmp_models_dir / model_name)
            inp_shape = strip_none_from_tensor_shape(model.input_shape)
            out_shape = strip_none_from_tensor_shape(model.output_shape)
            assert model.predict(np.zeros(shape=inp_shape).reshape(1, -1))[0].shape == out_shape
            assert isinstance(metainfo.train_mae, float)
            assert isinstance(metainfo.validation_mae, float)

            # -- NITTA-side ML node scoring --
            async def _get_scores(baseurl: str):
                async with ClientSession() as session:
                    tree = await retrieve_tree_root(baseurl, session)
                    await retrieve_children(tree, session, baseurl)
                assert tree.children is not None, "children should've been loaded"
                return [c.score for c in tree.children]

            async with run_nitta_server(EXAMPLES_DIR / "fibonacci.lua") as nitta:
                non_ml_scores = await _get_scores(await nitta.get_base_url())

            async with run_nitta_server(
                EXAMPLES_DIR / "fibonacci.lua",
                nitta_args="--score=does_not_exist --method=NoSynthesis",
            ) as nitta:
                fallback_non_ml_scores = await _get_scores(await nitta.get_base_url())
                assert non_ml_scores == fallback_non_ml_scores

            # TODO: fix NITTA not seeing the test model for some reason
            async with run_nitta_server(
                EXAMPLES_DIR / "fibonacci.lua",
                nitta_args=f'--score="ml_{model_name}"  --method=NoSynthesis -e',
                env={EnvVarNames.MODELS_DIR: tmp_model_dir_name},
            ) as nitta:
                ml_scores = await _get_scores(await nitta.get_base_url())
                assert non_ml_scores != ml_scores
