from __future__ import annotations

import asyncio
from argparse import ArgumentParser, Namespace

from components.common.logging import configure_logging, get_logger
from components.common.model_loading import load_model_with_metainfo
from components.data_crawling.data_crawling import crawl_data_from_many_examples
from components.model_generation.training import train_and_save_model
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

    model_name = "production"
    model_dir = MODELS_DIR / model_name
    try:
        model, meta = load_model_with_metainfo(model_dir)
        logger.info("Using manually built model")
        is_manual = True
    except (OSError, FileNotFoundError):
        is_manual = False

        model, meta = train_and_save_model(model_name=model_name)

    with (model_dir / "description.txt").open("w") as f:
        f.write(f"{'Manually' if is_manual else 'Automatically'} trained model for synthesis \n\n")
        f.write(f"Training MAE: {meta.train_mae:.3f}\n")
        f.write(f"Validation MAE: {meta.validation_mae:.3f}\n")

    logger.info(f"Done! Model saved to {model_dir}. Training history (+PNG chart), description and metainfo included.")
