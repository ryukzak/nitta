from __future__ import annotations

import asyncio
import shutil
from argparse import ArgumentParser, Namespace

from components.common.logging import configure_logging, get_logger
from components.common.model_loading import load_model_with_metainfo
from components.data_crawling.data_crawling import crawl_data_from_many_examples
from components.evaluation.rating import rate_evaluations
from components.model_generation.training import train_and_save_model
from consts import EVALUATION_CONFIGS_DIR, EVALUATIONS_DIR, MODELS_DIR
from scripts.evaluate_nitta_synthesis import (
    evaluate_nitta_synthesis,
    read_evaluation_config_from_json,
)

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
    argparser.add_argument(
        "--skip-evaluation",
        help="Do not run synthesis evaluation after training.",
        action="store_true",
    )

    return argparser.parse_args()


if __name__ == "__main__":
    configure_logging()

    args = _parse_args()

    model_name = "production"
    model_dir = MODELS_DIR / model_name
    try:
        model, meta = load_model_with_metainfo(model_dir)
        logger.info("Using manually built model")
        is_manual = True
    except (OSError, FileNotFoundError):
        is_manual = False

        if not args.skip_crawling:
            asyncio.run(crawl_data_from_many_examples())

        model, meta = train_and_save_model(model_name=model_name)

    rating_df = None
    if not args.skip_evaluation:
        config = read_evaluation_config_from_json(EVALUATION_CONFIGS_DIR / "ci.json")
        config.output_dir = EVALUATIONS_DIR / "ci"
        if config.output_dir.exists():
            shutil.rmtree(config.output_dir)

        evaluate_nitta_synthesis(config)
        rating_df = rate_evaluations(list(config.output_dir.glob("evaluation_*.csv")))

    with (model_dir / "description.txt").open("w") as f:
        f.write(f"{'Manually' if is_manual else 'Automatically'} trained model for synthesis \n\n")
        f.write(f"Training MAE: {meta.train_mae:.3f}\n")
        f.write(f"Validation MAE: {meta.validation_mae:.3f}\n")
        if rating_df is not None:
            f.write("\n---\n")
            # "tabulate" package is required for to_markdown()
            # preformat df for a decent appearance in markdown
            rating_df = rating_df.reset_index().drop("evaluation", axis=1)
            table = rating_df.to_markdown(tablefmt="github", index=False)
            f.write(f"Synthesis evaluation rating: \n{table}")

    logger.info(f"Done! Model saved to {model_dir}. Training history (+PNG chart), description and metainfo included.")
