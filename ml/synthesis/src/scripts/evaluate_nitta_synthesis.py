import asyncio
import itertools
import random
from asyncio.subprocess import DEVNULL, PIPE
from pathlib import Path
from time import perf_counter
from typing import Iterable, List, Tuple

import pandas as pd

from components.common.logging import configure_logging, get_logger
from components.data_crawling.example_running import run_nitta_raw
from components.data_crawling.saving import save_df_with_timestamp
from consts import EXAMPLES_DIR

logger = get_logger(__name__)

_output_dir = "evaluation"
_nitta_path = "stack exec nitta --"
_nitta_running_timeout_s = 120
_evaluated_args = {
    "score": {
        "default": "",
        "model_v1": "--score=ml_v1_final",
        "model_v2": "--score=ml_v2_exp2",
    },
}
examples = [*sorted(EXAMPLES_DIR.glob("**/*.lua"))]
_const_args = "-e"
_nitta_step_log_line_marker = "[DEBUG : NITTA.Synthesis] explore:"
_measurement_tries = 5


async def _run_config_and_save_results(
    results: List[dict],
    run_config: Iterable[Tuple[str, str, str]],
    example: Path,
):
    args_str = " ".join(opt_args for _, _, opt_args in run_config if opt_args)
    async with run_nitta_raw(
        cmd=f"{_nitta_path} {_const_args} {args_str} {example}",
        stdout=PIPE,
        stderr=DEVNULL,
    ) as nitta_proc:
        start_time = perf_counter()

        timeout = False
        try:
            await asyncio.wait_for(nitta_proc.wait(), timeout=_nitta_running_timeout_s)
        except asyncio.TimeoutError:
            logger.info(f"Timed out after {_nitta_running_timeout_s:.0f}s")
            timeout = True

        steps = 0
        line = await nitta_proc.stdout.readline()
        while line:
            decoded_line = line.decode("utf-8")
            # logger.debug(decoded_line)
            if _nitta_step_log_line_marker in decoded_line:
                steps += 1
            line = await nitta_proc.stdout.readline()

        results.append(
            {
                "example": example.name,
                "synthesis_steps": steps,
                **{param_name: opt_name for param_name, opt_name, _ in run_config},
                **({} if timeout else {"time": perf_counter() - start_time}),
            }
        )


async def main(results: List[dict]):
    search_space_dofs = [
        [(param_name, opt_name, opt_args) for opt_name, opt_args in param_opts.items()]
        for param_name, param_opts in _evaluated_args.items()
    ]

    runs = list(
        itertools.product(examples, range(_measurement_tries), *search_space_dofs)
    )
    random.shuffle(runs)
    for i, (example, measurement_try, *run_config) in enumerate(runs):
        # config: (param_name, opt_name, opt_args)[]

        params_readable_str = ", ".join(
            f"{param_name}={opt_name}" for param_name, opt_name, _ in run_config
        )
        logger.info(
            f">>> >>> >>> Processing {example} ({i+1} / {len(runs)}) >>> {params_readable_str}"
        )

        try:
            await _run_config_and_save_results(results, run_config, example)
            elapsed_time = results[-1].get("time")
            if elapsed_time is not None:
                logger.info(f"Finished in {elapsed_time:.2f}s")
        except Exception:
            logger.exception("Failed to run a config, skipping and continuing")


if __name__ == "__main__":
    configure_logging()

    results: List[dict] = []

    try:
        asyncio.run(main(results))
    except KeyboardInterrupt:
        logger.info("Interrupted by user, saving what we have")

    logger.info(f"Finished with {len(results)} results")
    df = pd.DataFrame(results)
    index_cols = ["example", *_evaluated_args.keys()]
    df = df.set_index(index_cols, drop=False)
    df = df.groupby(df.index).agg(
        {
            **{col: "sample" for col in index_cols},
            **{
                col: ["count", "mean", "std", "min", "max"]
                for col in df.columns
                if col not in index_cols
            },
        }
    )
    save_df_with_timestamp(
        df, _output_dir, "evaluation", what="evaluation results", index=False
    )
