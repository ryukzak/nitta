import asyncio
import os
import sys
from argparse import ArgumentParser
from pathlib import Path
from typing import List

from components.common.logging import configure_logging, get_logger
from components.data_crawling.example_running import (
    run_example_and_sample_tree_parallel,
)
from components.data_crawling.sampling_processing import (
    build_df_and_save_sampling_results,
    estimate_tree_coverage_based_on_clash_ratio,
    process_sampling_results,
)
from consts import DATA_DIR

_DEFAULT_N_SAMPLES = 5000
_DEFAULT_N_SAMPLES_PER_BATCH = 150
_DEFAULT_N_WORKERS = 1
_DEFAULT_N_NITTAS = 1
_DEFAULT_NITTA_EXE_PATH = "stack exec nitta --"

if __name__ == "__main__":
    logger = get_logger(__name__)
    configure_logging()

    argparser = ArgumentParser(
        prog="crawl_data_by_tree_sampling.py",
        description="Gathers training data for provided input file by sampling its synthesis tree.",
        epilog="It's recommended to adjust the number of samples to the size of the tree, as well as other parameters "
        + "to your machine's capabilities.",
    )
    argparser.add_argument(
        "file",
        help="Path to the input file for NITTA to synthesize.",
        type=Path,
    )
    argparser.add_argument(
        "-n",
        "--n-samples",
        help=f"Number of samples to gather. A sample is a full descent from root to leaf of the synthesis tree. "
        + f"Default: {_DEFAULT_N_SAMPLES} (adjust to tree size!).",
        default=_DEFAULT_N_SAMPLES,
        type=int,
    )
    argparser.add_argument(
        "-b",
        "--n-samples-per-batch",
        help=f"Number of samples to gather in a single batch. Default: {_DEFAULT_N_SAMPLES_PER_BATCH}.",
        default=_DEFAULT_N_SAMPLES_PER_BATCH,
        type=int,
    )
    argparser.add_argument(
        "-w",
        "--n-workers",
        help=f"Number of workers to use for parallel sampling. 1 recommended in most cases. Default: {_DEFAULT_N_WORKERS}.",
        default=_DEFAULT_N_WORKERS,
        type=int,
    )
    argparser.add_argument(
        "-i",
        "--n-nittas",
        help=f"Number of NITTA instances to run. 1 recommended in most cases. Default: {_DEFAULT_N_NITTAS}.",
        default=_DEFAULT_N_NITTAS,
        type=int,
    )
    argparser.add_argument(
        "--nitta-path",
        help=f"Path to the NITTA executable. Default: '{_DEFAULT_NITTA_EXE_PATH}' is used.",
        default=_DEFAULT_NITTA_EXE_PATH,
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
            args.file,
        ][0]
    )

    if not file_to_run.exists():
        logger.error(f"Couldn't find file '{file_to_run.absolute()}'.")
        sys.exit(1)

    samples_notice = (
        "(adjust to the tree size with -n N_SAMPLES!)"
        if args.n_samples == _DEFAULT_N_SAMPLES
        else ""
    )
    logger.info(
        f"Starting sampling of {args.file} with {args.n_samples} samples {samples_notice}"
    )

    results: List[dict] = []

    try:
        asyncio.run(
            run_example_and_sample_tree_parallel(
                file_to_run,
                n_samples=args.n_samples,
                n_samples_per_batch=args.n_samples_per_batch,
                n_workers=args.n_workers,
                n_nittas=args.n_nittas,
                nitta_exe_path=args.nitta_path,
                results_accum=results,
            )
        )
    except KeyboardInterrupt:

        def _no_traceback_excepthook(exc_type, exc_val, traceback):
            pass

        if sys.excepthook is sys.__excepthook__:
            sys.excepthook = _no_traceback_excepthook

        if len(results) == 0:
            raise

        logger.info("Interrupted by user, processing nodes gathered so far.")
    except Exception:
        logger.exception("Unexpected error, processing nodes gathered so far.")

    process_sampling_results(file_to_run, results)
