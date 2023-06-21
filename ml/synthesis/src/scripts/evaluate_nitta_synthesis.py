import asyncio
import itertools
import random
import re
import sys
from asyncio.subprocess import DEVNULL, PIPE
from pathlib import Path
from time import perf_counter
from typing import Iterable, List, Tuple

import pandas as pd

from components.common.logging import configure_logging, get_logger
from components.common.saving import save_df_with_timestamp
from components.data_crawling.example_running import run_nitta, run_nitta_raw
from components.data_crawling.node_processing import get_depth
from components.data_crawling.tree_retrieving import retrieve_single_node
from consts import EXAMPLES_DIR

logger = get_logger(__name__)

_output_dir = "evaluation"
_nitta_run_command = "stack exec nitta --"
_nitta_running_timeout_s = 60
_evaluated_args = {
    "score": {
        "default": "",
        "model_v1": "--score=ml_v1_final",
        "model_v2": "--score=ml_v2_exp2",
    },
    "synthesis_method": {
        "sota": "--synthesis-method=stateOfTheArtSynthesisIO",
        "top-down-1.2": "--synthesis-method=topDownScoreSynthesisIO --depth-base=1.2",
        "top-down-1.4": "--synthesis-method=topDownScoreSynthesisIO --depth-base=1.4",
    },
}
examples = [*sorted(EXAMPLES_DIR.glob("**/*.lua"))]
# examples = [EXAMPLES_DIR / "fibonacci.lua"]
_const_args = "-e"
_measurement_tries = 3


async def _run_config_and_save_results(
    results: List[dict],
    run_config: Iterable[Tuple[str, str, str]],
    example: Path,
):
    args_str = " ".join(opt_args for _, _, opt_args in run_config if opt_args)
    async with run_nitta_raw(
        cmd=f"{_nitta_run_command} {_const_args} {args_str} {example}",
        stdout=PIPE,
        stderr=DEVNULL,
    ) as nitta_proc:
        start_time = perf_counter()

        success = False
        timeout = False
        steps = 0
        last_sid = ""
        explore_regexp = re.compile(
            r"^\[DEBUG : NITTA\.Synthesis\] explore: (?P<sid>(-\d+)+)"
        )

        line = await nitta_proc.stdout.readline()
        while line:
            decoded_line = line.decode("utf-8")

            explore_match = explore_regexp.match(decoded_line)
            if explore_match:
                steps += 1
                last_sid = explore_match.group("sid")

            if "synthesis process...ok" in decoded_line:
                success = True
                break

            line = None
            while not line:
                if (perf_counter() - start_time) > _nitta_running_timeout_s:
                    logger.info(f"Timed out after {_nitta_running_timeout_s:.0f}s")
                    timeout = True
                    break
                try:
                    line = await asyncio.wait_for(
                        nitta_proc.stdout.readline(), timeout=1
                    )
                except asyncio.TimeoutError:
                    pass

        logger.debug(
            f"Done parsing stdout, success={success}, timeout={timeout}, steps={steps}"
        )

        elapsed_time = perf_counter() - start_time
        if last_sid:
            async with run_nitta(example) as (_, nitta_baseurl):
                node = await retrieve_single_node(nitta_baseurl, last_sid)

        results.append(
            {
                "example": example.name,
                "success": int(success),
                "timeout": int(timeout),
                **{param_name: opt_name for param_name, opt_name, _ in run_config},
                **(
                    {}
                    if timeout
                    else {
                        "time": elapsed_time,
                        "synthesis_steps": steps,
                    }
                ),
                **(
                    {}
                    if not success
                    else {
                        "result_depth": get_depth(last_sid),
                        "duration": node.duration,
                    }
                ),
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
    if not results:
        sys.exit(0)

    df = pd.DataFrame(results)
    index_cols = ["example", *_evaluated_args.keys()]
    counter_cols = ["success", "timeout"]
    df = df.set_index(index_cols, drop=False)
    df = df.groupby(df.index).agg(
        {
            "example": ["sample", "count"],
            **{col: ["sample"] for col in index_cols if col != "example"},
            **{col: ["mean", "sum"] for col in counter_cols},
            **{
                col: ["mean", "std", "min", "max"]
                for col in df.columns
                if col not in index_cols + counter_cols
            },
        }
    )
    save_df_with_timestamp(
        df, _output_dir, "evaluation", what="evaluation results", index=False
    )
