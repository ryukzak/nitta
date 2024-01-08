from __future__ import annotations

from pathlib import Path

import pandas as pd


def rate_evaluations(evaluation_paths: list[Path]):
    eval_dfs = [pd.read_csv(path) for path in evaluation_paths]
    rating_dfs = []

    for eval_df in eval_dfs:
        eval_df = eval_df.drop(["example"], axis=1)
        key_cols = eval_df.columns[: eval_df.columns.get_loc("runs")]
        eval_df = eval_df[[*key_cols, "success_rate", "time_mean"]]

        # time = 0 seconds => time_rating = 1
        # time = <pivot> seconds => time_rating = 0
        time_pivot = 60

        eval_df["time_rating"] = eval_df["time_mean"].apply(
            lambda t: 0 if pd.isna(t) else (time_pivot - t) / time_pivot,
        )
        eval_df["rating"] = eval_df["success_rate"] * 0.5 + eval_df["time_rating"] * 0.5

        eval_df = eval_df.groupby(key_cols.to_list()).mean()

        eval_df = eval_df[["rating"]]
        eval_df = (eval_df * 100).round(0).astype(int)

        rating_dfs.append(eval_df)

    evaluation_names = [p.stem for p in evaluation_paths]
    final_df = pd.concat(rating_dfs, keys=evaluation_names, names=["evaluation"])
    final_df = final_df.sort_values("rating", ascending=False)

    return final_df
