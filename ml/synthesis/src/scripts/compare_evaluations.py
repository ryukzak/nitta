import logging
from argparse import ArgumentParser, Namespace
from pathlib import Path

import pandas as pd

from components.common.logging import configure_logging, get_logger

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

    eval_dfs = [pd.read_csv(path) for path in args.evaluations]
    rating_dfs = []

    for eval_df in eval_dfs:
        eval_df = eval_df.drop(["example"], axis=1)
        key_cols = eval_df.columns[: eval_df.columns.get_loc("runs")]
        eval_df = eval_df[[*key_cols, "success_rate", "time_mean"]]

        eval_df["time_rating"] = eval_df["time_mean"].apply(lambda t: 0 if t == 0 else (60 - t) / 60)
        eval_df["rating"] = eval_df["success_rate"] * 0.5 + eval_df["time_rating"] * 0.5

        eval_df = eval_df.groupby(key_cols.to_list()).mean()

        eval_df = eval_df[["rating"]]
        eval_df = (eval_df * 100).round(0).astype(int)

        rating_dfs.append(eval_df)

    evaluation_names = [p.stem for p in args.evaluations]
    final_df = pd.concat(rating_dfs, keys=evaluation_names, names=["evaluation"])
    final_df = final_df.sort_values("rating", ascending=False)
    print(final_df.to_string(sparsify=False))  # noqa: T201
