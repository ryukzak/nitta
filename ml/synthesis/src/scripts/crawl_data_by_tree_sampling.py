import asyncio
import sys
from pathlib import Path

from components.common.logging import configure_logging, get_logger
from components.data_crawling.example_running import (
    run_example_and_sample_tree_parallel,
)

if __name__ == "__main__":
    logger = get_logger(__name__)
    configure_logging()

    examples = [
        # "examples/fibonacci.lua",
        # "examples/spi2.lua",
        # "examples/counter.lua",
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

    asyncio.run(
        run_example_and_sample_tree_parallel(
            Path(examples[0]), n_samples=50000, n_workers=2, n_nittas=1
        )
    )
