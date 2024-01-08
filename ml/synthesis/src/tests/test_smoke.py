from pathlib import Path
from tempfile import TemporaryDirectory

import numpy as np
from aiohttp import ClientSession

from components.common.data_loading import load_all_existing_training_data
from components.common.logging import get_logger
from components.common.model_loading import load_model_with_metainfo
from components.data_crawling.data_crawling import crawl_data_from_example
from components.data_crawling.nitta.nitta_running import run_nitta_server
from components.data_crawling.nitta.tree_retrieving import retrieve_children, retrieve_tree_root
from components.data_processing.dataset_creation import TARGET_COLUMNS, create_datasets
from components.data_processing.feature_engineering import preprocess_input_data_df
from components.model_generation.training import train_and_save_model_on_given_data
from components.utils.tensorflow import strip_none_from_tensor_shape
from consts import EXAMPLES_DIR, ML_BACKEND_BASE_URL_FILEPATH, ROOT_DIR, EnvVarNames

logger = get_logger(__name__)


async def test_smoke():
    with TemporaryDirectory() as tmp_data_dir_name:
        with TemporaryDirectory() as tmp_models_dir_name:
            # -- model generation pipeline --
            tmp_data_dir = Path(tmp_data_dir_name)
            tmp_models_dir = Path(tmp_models_dir_name)

            await crawl_data_from_example(EXAMPLES_DIR / "fibonacci.lua", data_dir=tmp_data_dir)
            df = load_all_existing_training_data(tmp_data_dir)
            assert df[TARGET_COLUMNS].dropna().size > 0, "labels weren't calculated"

            pdf = preprocess_input_data_df(df)
            tds, vds, input_cols = create_datasets(pdf)

            model_name = "test_model"
            train_and_save_model_on_given_data(
                tds,
                vds,
                input_cols,
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

            base_url_file = ROOT_DIR / ML_BACKEND_BASE_URL_FILEPATH
            assert not base_url_file.exists(), (
                f"Unexpected {base_url_file.resolve().as_posix()!r} exists, so this test will fail. Is something"
                + " running there? Remove it and re-run the test. Not doing it automatically not to break stuff."
            )

            async with run_nitta_server(
                EXAMPLES_DIR / "fibonacci.lua",
                nitta_args="--score=does_not_exist --method=NoSynthesis",
            ) as nitta:
                fallback_non_ml_scores = await _get_scores(await nitta.get_base_url())
                assert non_ml_scores == fallback_non_ml_scores

            async with run_nitta_server(
                EXAMPLES_DIR / "fibonacci.lua",
                nitta_args=f'--score="ml_{model_name}"  --method=NoSynthesis -e',
                env={EnvVarNames.MODELS_DIR: tmp_models_dir.resolve()},
            ) as nitta:
                ml_scores = await _get_scores(await nitta.get_base_url())
                assert non_ml_scores != ml_scores
