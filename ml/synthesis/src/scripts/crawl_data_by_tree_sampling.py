import asyncio
import sys
from pathlib import Path

import pandas as pd

from components.common.logging import configure_logging, get_logger
from components.data_crawling.example_running import (
    run_example_and_sample_tree_parallel,
)
from consts import DATA_DIR

if __name__ == "__main__":
    logger = get_logger(__name__)
    configure_logging()

    examples = [
        # "examples/fibonacci.lua",
        # "examples/spi2.lua",
        "examples/counter.lua",
        # "examples/sum.lua",  # too slow
        # "examples/constantFolding.lua",  # too much memory needed
        # "examples/spi3.lua",  # too much memory needed
        # "examples/teacup.lua",  # too much memory needed
        "examples/pid.lua",  # too much memory needed
    ]
    if len(sys.argv) > 1:
        examples = sys.argv[1:]
    else:
        logger.info(f"Used default examples {examples}")

    results = []

    for n_samples in sum(
        [
            [50000] * 1,
            [25000] * 1,
            [10000] * 1,
            [5000] * 1,
            [2500] * 1,
            [2000] * 1,
            [1500] * 1,
            [1300] * 2,
            [1200] * 2,
            [1100] * 2,
            [1000] * 3,
            [900] * 3,
            [800] * 4,
            [700] * 4,
            [600] * 5,
            [500] * 5,
            [400] * 6,
            [300] * 6,
            [250] * 7,
            [200] * 7,
            [150] * 8,
            [100] * 8,
            [75] * 8,
            [50] * 8,
            [25] * 8,
            [15] * 8,
            [10] * 8,
            [5] * 8,
            [1] * 8,
        ],
        [],
    ):
        (n_unique_sids, node_clash_percent) = asyncio.run(
            run_example_and_sample_tree_parallel(
                Path(examples[0]), n_samples=n_samples, n_workers=1, n_nittas=1
            )
        )
        logger.info(
            f"n_samples,uniq,clash: {n_samples}\t{n_unique_sids}\t{node_clash_percent}"
        )
        results.append(
            {
                "n_samples": n_samples,
                "n_unique_sids": n_unique_sids,
                "node_clash_percent": node_clash_percent,
            }
        )

    pd.DataFrame(results).to_csv(DATA_DIR / "treecov_estimates.csv", index=False)
