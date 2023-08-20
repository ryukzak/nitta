from argparse import ArgumentParser
from pathlib import Path

from components.common.logging import configure_logging, get_logger
from components.data_crawling.data_crawling import crawl_data_from_example
from components.data_crawling.tree_sampling import (
    DEFAULT_N_NITTAS,
    DEFAULT_N_SAMPLES,
    DEFAULT_N_SAMPLES_PER_BATCH,
    DEFAULT_N_WORKERS,
    DEFAULT_NITTA_RUN_COMMAND,
)
from components.utils.asyncio_run_interrupt_wrapper import asyncio_run_safe

if __name__ == "__main__":
    logger = get_logger(__name__)
    configure_logging()

    argparser = ArgumentParser(
        prog="crawl_data_by_tree_sampling.py",
        description="Gathers training data by sampling the synthesis tree of provided target algorithm.",
        epilog="It's recommended to adjust the number of samples to the size of the tree, as well as other parameters "
        + "to your machine's capabilities.",
    )
    argparser.add_argument(
        "input_file",
        help="Path to the target algorithm description for NITTA to synthesize.",
        type=Path,
    )
    argparser.add_argument(
        "-n",
        "--n-samples",
        help="Number of samples to gather. A sample is a full descent from root to leaf in the synthesis tree. "
        + f"Default: {DEFAULT_N_SAMPLES} (adjust to tree size!).",
        default=DEFAULT_N_SAMPLES,
        type=int,
    )
    argparser.add_argument(
        "-b",
        "--n-samples-per-batch",
        help="Number of samples in a single batch (batch size). The sampling process is batched for better "
        + "performance: each worker's one-time job is to (asynchronously) process a whole batch, not an individual "
        + f"sample. Default batch size: {DEFAULT_N_SAMPLES_PER_BATCH}.",
        default=DEFAULT_N_SAMPLES_PER_BATCH,
        type=int,
    )
    argparser.add_argument(
        "-w",
        "--n-workers",
        help="Number of worker processes to use for parallel sampling. 1 recommended in most cases. "
        + f"Default: {DEFAULT_N_WORKERS}.",
        default=DEFAULT_N_WORKERS,
        type=int,
    )
    argparser.add_argument(
        "-i",
        "--n-nittas",
        help=f"Number of NITTA instances to run. 1 recommended in most cases. Default: {DEFAULT_N_NITTAS}.",
        default=DEFAULT_N_NITTAS,
        type=int,
    )
    argparser.add_argument(
        "--nitta-run-command",
        help="A command to run NITTA (or a path to the NITTA executable, same thing). "
        + f"Default: '{DEFAULT_NITTA_RUN_COMMAND}'.",
        default=DEFAULT_NITTA_RUN_COMMAND,
        type=str,
    )

    args = argparser.parse_args()

    # list of uncommentable examples for development convenience, top-1 will be taken
    file_to_run = Path(
        [
            # >>>>>>> small: >>>>>>>
            # "examples/fibonacci.lua",
            # "examples/spi2.lua",
            # "examples/sum.lua",
            # "examples/counter.lua",
            # >>>>>>> large: >>>>>>>
            # "examples/constantFolding.lua",
            # "examples/spi3.lua",
            # "examples/teacup.lua",
            # "examples/pid.lua",
            # "examples/sin_ident/sin_ident.lua",
            # >>>>>>> unknown size: >>>>>>>
            # "examples/shift.lua",
            # "examples/spi1.lua",
            # "examples/double_receive.lua",
            args.input_file,
        ][0],
    )

    if args.n_samples == DEFAULT_N_SAMPLES:
        logger.info(
            "Sampling till a default number of samples is gathered. Adjust to synthesis tree size with -n N_SAMPLES!",
        )

        asyncio_run_safe(
            crawl_data_from_example(
                file_to_run,
                n_samples=args.n_samples,
                n_samples_per_batch=args.n_samples_per_batch,
                n_workers=args.n_workers,
                n_nittas=args.n_nittas,
                nitta_run_command=args.nitta_run_command,
            ),
        )
