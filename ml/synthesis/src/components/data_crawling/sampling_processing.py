from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path

import pandas as pd

from components.common.logging import get_logger
from components.common.saving import save_df_with_timestamp
from components.data_crawling.node_label_computation import aggregate_node_labels
from components.data_crawling.tree_sampling_coverage_estimation import estimate_tree_coverage_based_on_collision_ratio
from consts import DATA_DIR

logger = get_logger(__name__)


@dataclass
class SamplingStats:
    n_results: int
    n_unique_sids: int
    collision_ratio: float
    tree_coverage: float | None
    neg_label_share: float
    # ^ all floats are in [0, 1]


@dataclass
class SamplingResult:
    example: Path
    df: pd.DataFrame
    stats: SamplingStats


def process_and_save_sampling_results(
    example: Path,
    results: list[dict],
    data_dir: Path = DATA_DIR,
) -> SamplingResult | None:
    if len(results) == 0:
        logger.warning(f"No results to process (example {example.name!r}).")
        return None

    results_df = _build_df_and_save_sampling_results(results, example.name, data_dir)

    n_results = len(results)
    n_unique_sids = len(results_df.index.unique())  # already deduplicated
    collision_ratio = n_results / n_unique_sids - 1
    tree_cov = estimate_tree_coverage_based_on_collision_ratio(collision_ratio, n_unique_sids)
    tree_cov_str = f"{tree_cov * 100:.3f}%" if tree_cov else "unknown :("
    neg_label_share = (results_df.label == -3).sum() / len(results_df)
    logger.info(
        "GOT TREE SAMPLING RESULTS:\n\t"
        + ",\n\t".join(
            [
                f" {n_unique_sids} unique nodes",
                f" collision ratio: {collision_ratio * 100:.3f}%",
                f" neg. label: {neg_label_share * 100:.1f}%",
                f" estimated tree coverage: {tree_cov_str}",
            ],
        ),
    )

    return SamplingResult(
        example=example,
        df=results_df,
        stats=SamplingStats(
            n_results=n_results,
            n_unique_sids=n_unique_sids,
            collision_ratio=collision_ratio,
            tree_coverage=tree_cov,
            neg_label_share=neg_label_share,
        ),
    )


def _build_df_and_save_sampling_results(results: list[dict], example_name: str, data_dir: Path) -> pd.DataFrame:
    """
    Builds a DataFrame from the sampling results and saves it to the `data_dir`.
    """
    logger.info(f"Building a DataFrame from sampling results ({len(results)} entries)...")

    results_df = pd.DataFrame(results)

    # there may be significant duplication of data in results at this point.
    # clashing nodes differ only in labels.
    # any way to improve this? seems extremely difficult (parallel executions won't be independent)

    results_df = results_df.set_index("sid", drop=True)
    results_df_labels = results_df.groupby(results_df.index).label.agg(aggregate_node_labels)

    results_df = results_df[~results_df.index.duplicated(keep="first")]
    results_df.label = results_df_labels

    save_df_with_timestamp(results_df, data_dir, f"{example_name}.sampling", what="results")

    return results_df
