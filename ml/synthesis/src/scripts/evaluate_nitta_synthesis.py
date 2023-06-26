import asyncio
import itertools
import logging
import random
import sys
from pathlib import Path
from time import perf_counter
from typing import Iterable, List, Tuple

import pandas as pd
from aiohttp import ClientSession

from components.common.logging import configure_logging, get_logger
from components.common.saving import save_df_with_timestamp
from components.data_crawling.nitta_running import run_nitta_server
from components.data_crawling.tree_retrieving import retrieve_tree_info
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
        "sota": "--method=StateOfTheArt",
        "top-down-1.2": "--method=TopDownByScore --depth-base=1.2",
        "top-down-1.4": "--method=TopDownByScore --depth-base=1.4",
    },
}
examples = [*sorted(EXAMPLES_DIR.glob("**/*.lua"))]
# examples = [EXAMPLES_DIR / "fibonacci.lua"]
_const_args = "-e"
_measurement_tries = 3

_TIMEOUT_CHECKING_INTERVAL_S = 2


async def _run_config_and_save_results(
    results: List[dict],
    run_config: Iterable[Tuple[str, str, str]],
    example: Path,
):
    args_str = " ".join(opt_args for _, _, opt_args in run_config if opt_args)
    async with run_nitta_server(
        example=example,
        nitta_run_command=_nitta_run_command,
        nitta_args=f"{_const_args} {args_str}",
    ) as nitta:
        start_time = perf_counter()

        nitta_base_url = None
        elapsed_time = 0.0
        while not nitta_base_url and elapsed_time < _nitta_running_timeout_s:
            try:
                nitta_base_url = await asyncio.wait_for(
                    nitta.get_base_url(), timeout=_TIMEOUT_CHECKING_INTERVAL_S
                )
            except asyncio.TimeoutError:
                pass
            elapsed_time = perf_counter() - start_time

        timeout = elapsed_time >= _nitta_running_timeout_s
        success = False
        stats = {}
        if timeout:
            logger.info(f"Timeout of {_nitta_running_timeout_s}s reached.")
        elif nitta_base_url is None:
            logger.info(
                f"A error occurred at {elapsed_time:.2f}s, so it's neither a timeout, nor a success."
            )
        else:
            logger.info(
                f"Synthesis done, NITTA API server started on {nitta_base_url}. "
                f"Elapsed time: {elapsed_time:.2f}s. Getting tree info..."
            )
            async with ClientSession() as session:
                ti = await retrieve_tree_info(nitta_base_url, session)

            logger.info(f"Got tree info: {ti}")

            def _mean_from_tree_info_dict(d: dict) -> float:
                # dicts are like {value: node_count}, we need a weighted mean
                # {"13": 3, "9": 6} -> (3*13 + 6*9) / (3+6)
                if sum(d.values()) == 0:
                    return 0
                return sum(int(k) * v for k, v in d.items()) / sum(d.values())

            success = ti.success > 0
            stats["time"] = elapsed_time
            stats["synthesis_steps"] = ti.nodes - ti.not_processed - 1  # -1 for root
            stats["mean_depth"] = _mean_from_tree_info_dict(ti.steps_success)
            stats["min_duration"] = min(int(v) for v in ti.duration_success.values())
            stats["leafs"] = ti.success + ti.failed
            stats["leaf_success_rate"] = ti.success / stats["leafs"]
            stats["nodes_total"] = ti.nodes
            stats["nodes_not_processed"] = ti.not_processed

        results.append(
            {
                "example": example.name,
                "success": int(success),
                "timeout": int(timeout),
                **{param_name: opt_name for param_name, opt_name, _ in run_config},
                **stats,
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
    configure_logging(logging.INFO)

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
