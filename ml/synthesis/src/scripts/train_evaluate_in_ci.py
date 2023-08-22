from __future__ import annotations

import asyncio
from argparse import ArgumentParser, Namespace

from components.common.data_loading import load_all_existing_training_data
from components.common.logging import configure_logging, get_logger
from components.common.model_loading import load_model_with_metainfo
from components.data_crawling.data_crawling import crawl_data_from_many_examples
from components.data_processing.dataset_creation import create_datasets
from components.data_processing.feature_engineering import preprocess_input_data_df
from components.model_generation.training import train_and_save_baseline_model
from consts import MODELS_DIR

logger = get_logger(__name__)


def _parse_args() -> Namespace:
    argparser = ArgumentParser(
        prog="train_evaluate_in_ci.py",
    )
    argparser.add_argument(
        "--skip-crawling",
        help="Do not crawl new training data, try to use existing.",
        action="store_true",
    )

    return argparser.parse_args()


if __name__ == "__main__":
    configure_logging()

    args = _parse_args()

    if not args.skip_crawling:
        asyncio.run(crawl_data_from_many_examples())

    model_dir = MODELS_DIR / "production"
    try:
        model, meta = load_model_with_metainfo(model_dir)
        logger.info("Using manually built model")
        is_manual = True
    except (OSError, FileNotFoundError):
        logger.info("Training model from scratch")
        is_manual = False

        training_data = load_all_existing_training_data()
        preprocessed = preprocess_input_data_df(training_data)
        train_ds, val_ds, input_cols = create_datasets(preprocessed)

        model, meta = train_and_save_baseline_model(train_ds, val_ds, input_cols, output_model_name="production")

    with (model_dir / "description.txt").open("w") as f:
        f.write(f"{'Manually' if is_manual else 'Automatically'} trained model for synthesis \n\n")
        f.write(f"Training MAE: {meta.train_mae:.3f}\n")
        f.write(f"Validation MAE: {meta.validation_mae:.3f}\n")

    logger.info(f"Done! Model saved to {model_dir}. Training history (+PNG chart), description and metainfo included.")
