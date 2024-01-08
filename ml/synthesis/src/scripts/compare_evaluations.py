import logging
from argparse import ArgumentParser, Namespace
from pathlib import Path

from components.common.logging import configure_logging, get_logger
from components.evaluation.rating import rate_evaluations

logger = get_logger(__name__)


def _parse_args() -> Namespace:
    argparser = ArgumentParser(
        prog="compare_evaluations.py",
        description="Computes a basic rating of different evaluations, prints it for comparison. ",
        epilog="Rating depends on synthesis success rate and time. It is roughly between 0 and 100."
        + "It's evaluated per-example and then averaged over all examples present in evaluation.",
    )

    argparser.add_argument(
        "evaluations",
        nargs="+",
        help="Paths to CSV files with evaluation results.",
        type=Path,
    )

    return argparser.parse_args()


if __name__ == "__main__":
    configure_logging(logging.WARNING)
    args = _parse_args()
    final_df = rate_evaluations(args.evaluations)
    print(final_df.to_string(sparsify=False))  # noqa: T201
