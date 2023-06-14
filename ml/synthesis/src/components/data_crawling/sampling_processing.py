import os
from math import exp, log
from pathlib import Path
from typing import List, Optional

import pandas as pd

from components.common.logging import get_logger
from components.common.saving import save_df_with_timestamp
from components.data_crawling.node_label_computation import aggregate_node_labels
from consts import DATA_DIR

logger = get_logger(__name__)


def build_df_and_save_sampling_results(
    results: List[dict], example_name: str, data_dir: Path
) -> pd.DataFrame:
    """
    Builds a DataFrame from the sampling results and saves it to the `data_dir`.
    """
    logger.info(
        f"Building a DataFrame from sampling results ({len(results)} entries)..."
    )

    results_df = pd.DataFrame(results)

    # there may be significant duplication of data in results at this point.
    # clashing nodes differ only in labels.
    # any way to improve this? seems extremely difficult (parallel executions won't be independent)

    results_df = results_df.set_index("sid", drop=True)
    results_df_labels = results_df.groupby(results_df.index).label.agg(
        aggregate_node_labels
    )

    results_df = results_df[~results_df.index.duplicated(keep="first")]
    results_df.label = results_df_labels

    save_df_with_timestamp(
        results_df, data_dir, f"{example_name}.sampling", what="results"
    )

    return results_df


def estimate_tree_coverage_based_on_clash_ratio(
    clash_ratio: float, n_nodes: int
) -> Optional[float]:
    """
    This is a bit sketchy yet state-of-the-art ad hoc heuristic estimation of tree coverage based on clash ratio.

    It's based on the following hypotheses:
        1) tree coverage is in pure functional (mathematical) dependency on clash ratio
        2) this dependency is tree-independent and is explained purely by random descent algorithm definition,
           probability theory and statistics.

    All approximation functions and coefficients are derived empirically from a set of sampling runs.
    All formulas are arbitrarily chosen to mathematically describe empirical data as close as my math skills allow.

    Error is estimated to be up to 10-15%, but it may depend on the tree peculiarities.

    UPD: found not suitable for large trees (leading to very small cron value) :(
    More data and runs needed, but I'm out of time.
    """
    # clash ratio in percents with a small epsilon to avoid division by zero
    cr = clash_ratio * 100
    n = n_nodes  # number of unique nodes sampled from the tree
    cron = cr / n  # short for "cr over n". like cr, but independent of n.

    if cron < 0.02:
        return None

    # nr - tree coverage, i.e. number of nodes sampled from the tree in this run

    # goal - find "approx(cron*nr)", i.e. approximate value of cron*nr
    # empirical data shows that approximation function depends on cr <> 100% heavily

    # approximation function for cr < 100%, found empirically
    approx_lt100 = -0.019 * log(cron) - 0.0394

    # approximation function for cr > 100%, found empirically
    approx_gt100 = 1.0225 * cron - 0.0288

    # we will blend these two functions with a sigmoid
    blender_width_coef = 0.02  # found empirically
    blender_center = 100.0
    blender_value = 1 / (1 + exp(-blender_width_coef * (cr - blender_center)))

    # blend the two approximation functions (as sigmoid goes 0..1, we go lt100..gt100)
    approx = approx_lt100 * (1 - blender_value) + approx_gt100 * blender_value

    # approx = cron * nr
    approx_nr = max(0, min(1, approx / cron))

    return approx_nr


def process_sampling_results(
    example: Path, results: List[dict], data_dir: Path = DATA_DIR
):
    example_name = os.path.basename(example)
    results_df = build_df_and_save_sampling_results(results, example_name, data_dir)

    n_results = len(results)
    n_unique_sids = len(results_df.index.unique())  # already deduplicated
    clash_ratio = n_results / n_unique_sids - 1
    tree_cov = estimate_tree_coverage_based_on_clash_ratio(clash_ratio, n_unique_sids)
    tree_cov_str = f"{tree_cov * 100:.3f}%" if tree_cov else "unknown :("
    neg_label_share = (results_df.label == -3).sum() / len(results_df)
    logger.info(
        f"TREE SAMPLING DONE:\n\t"
        + ",\n\t".join(
            [
                f" {n_unique_sids} unique nodes",
                f" clash ratio: {clash_ratio * 100:.3f}%",
                f" neg. label: {neg_label_share * 100:.1f}%",
                f" estimated tree coverage: {tree_cov_str}",
            ]
        )
    )

    stats = {
        "example": example_name,
        "n_results": n_results,
        "n_unique_sids": n_unique_sids,
        "clash_ratio": clash_ratio,
        "tree_coverage": tree_cov,
        "neg_label_share": neg_label_share,
    }

    return results_df, stats
