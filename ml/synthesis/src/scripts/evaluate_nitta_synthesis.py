"""
A script for evaluation of NITTA's synthesis. It runs NITTA with different arguments, measures metrics (like time, 
synthesis success rate, etc.) and writes aggregated results to a single CSV table for further analysis.

!!! Do not add non-stdlib imports here and be compatible with Python 3.8 !!!

By design, this script can be run WITHOUT any non-stdlib dependencies, so it doesn't require installation, just 
a compatible Python (3.8+ should be fine). This enables evaluating NITTA without full Python-ML stack installed. So be 
careful when adding additional imports here or in modules that this script depends on.
"""
import asyncio
import itertools
import json
import logging
import operator
import pickle
import random
import sys
from argparse import ArgumentParser, Namespace
from collections import defaultdict
from dataclasses import dataclass, fields
from datetime import datetime
from functools import reduce
from pathlib import Path
from statistics import mean, stdev
from time import perf_counter
from typing import Callable, Dict, Iterable, List, Literal, Set, Tuple, Union
from urllib.request import urlopen

from components.common.logging import configure_logging, get_logger
from components.common.saving import save_dicts_list_to_csv_with_timestamp
from components.data_crawling.nitta_running import run_nitta_server
from consts import EXAMPLES_DIR

logger = get_logger(__name__)

# left out of the config since considered not that important
_TIMEOUT_CHECKING_INTERVAL_S = 2


@dataclass
class EvaluationConfig:
    # raw values from the config file / CLI args
    output_dir: str
    nitta_run_command: str
    nitta_run_timeout_s: int
    measurement_tries: int
    examples: Union[List[str], Literal["all"]]
    constant_args: str
    evaluated_args: Dict[str, Dict[str, str]]


def _build_config(config_path: Path, args) -> EvaluationConfig:
    with config_path.open() as f:
        config = json.load(f)

    for arg, value in vars(args).items():
        if arg in config and value is not None:
            config[arg] = value

    return EvaluationConfig(**config)


def _build_argparser() -> ArgumentParser:
    argparser = ArgumentParser(
        prog="evaluate_nitta_synthesis.py",
        description="Runs NITTA synthesis multiple times with different arguments specified in evaluation config, "
        + "records result metrics and produces an aggregated results table.",
        epilog="Evaluation configuration is mostly done via a JSON config file (1st positional and the only required "
        + "CLI argument). Other CLI arguments are provided as a convenient way to override simple config values.",
    )
    argparser.add_argument(
        "config",
        type=Path,
        help="Path to the evaluation JSON config file (see examples in evaluation_configs).",
    )
    argparser.add_argument(
        "--with-ml-backend",
        action="store_true",
        help="If specified, keeps a single ML backend server running to reuse it during synthesis evaluation. "
        + "Otherwise, NITTA will start/stop a new server for each synthesis run.",
    )
    # iterate fields in EvaluationConfig and add them to the argparser if type is in (int, str)
    for field in fields(EvaluationConfig):
        if field.type in (int, str):
            cli_field_name = field.name.replace("_", "-")
            argparser.add_argument(
                f"--{cli_field_name}",
                help=f"Overrides the value of {field.name!r} from the config file.",
                type=field.type,
                metavar=field.type.__name__.upper(),
                required=False,
            )
    return argparser


# this class is duplicated because this script should work without non-stdlib dependencies
@dataclass
class _NittaTreeInfo:
    nodes: int
    success: int
    failed: int
    not_processed: int
    duration_success: Dict[str, int]
    steps_success: Dict[str, int]


def _get_tree_info_from_nitta(nitta_base_url: str) -> _NittaTreeInfo:
    with urlopen(nitta_base_url + "/treeInfo") as resp:
        ti_dict = json.loads(resp.read().decode("utf8"))

    # manually handle case differences
    ti_dict["not_processed"] = ti_dict.pop("notProcessed")
    ti_dict["duration_success"] = ti_dict.pop("durationSuccess")
    ti_dict["steps_success"] = ti_dict.pop("stepsSuccess")

    return _NittaTreeInfo(**ti_dict)


async def _assemble_stats_dict_after_synthesis(
    nitta_base_url: str, elapsed_time: float
) -> Tuple[dict, _NittaTreeInfo]:
    stats: dict = {}

    ti = _get_tree_info_from_nitta(nitta_base_url)

    logger.info(f"Got tree info: {ti}")

    if not ti.success > 0:
        logger.info(f"Synthesis was not successful, skipping stats collection")
        return stats, ti

    def _mean_from_tree_info_dict(d: dict) -> float:
        # dicts are like {value: node_count}, we need a weighted mean
        # {"13": 3, "9": 6} -> (3*13 + 6*9) / (3+6)
        if sum(d.values()) == 0:
            return 0
        return sum(int(k) * v for k, v in d.items()) / sum(d.values())

    stats["time"] = elapsed_time
    stats["synthesis_steps"] = ti.nodes - ti.not_processed - 1  # -1 for root
    stats["mean_depth"] = _mean_from_tree_info_dict(ti.steps_success)
    stats["min_duration"] = min(int(k) for k in ti.duration_success.keys())
    stats["leafs"] = ti.success + ti.failed
    stats["leaf_success_rate"] = ti.success / (ti.success + ti.failed)
    stats["nodes_total"] = ti.nodes
    stats["nodes_not_processed"] = ti.not_processed

    return stats, ti


async def _do_a_run_and_save_results(
    results: List[dict],
    config: EvaluationConfig,
    run_info: Iterable[Tuple[str, str, str]],
    example: Path,
):
    args_str = " ".join(opt_args for _, _, opt_args in run_info if opt_args)
    async with run_nitta_server(
        example=example,
        nitta_run_command=config.nitta_run_command,
        nitta_args=f"{config.constant_args} {args_str}",
    ) as nitta:
        start_time = perf_counter()

        nitta_base_url = None
        elapsed_time = 0.0
        while not nitta_base_url and elapsed_time < config.nitta_run_timeout_s:
            try:
                nitta_base_url = await asyncio.wait_for(
                    nitta.get_base_url(), timeout=_TIMEOUT_CHECKING_INTERVAL_S
                )
            except asyncio.TimeoutError:
                pass
            elapsed_time = perf_counter() - start_time

        timeout = elapsed_time >= config.nitta_run_timeout_s
        success = False
        stats: dict = {}
        if timeout:
            logger.info(f"Timeout of {config.nitta_run_timeout_s}s reached.")
        else:
            assert (
                nitta_base_url is not None
            ), "it's not a timeout, so NITTA server should've been started. Control flow bug?"
            logger.info(
                f"Detected a NITTA API server on {nitta_base_url}. Getting tree info..."
            )
            stats, ti = await _assemble_stats_dict_after_synthesis(
                nitta_base_url, elapsed_time
            )
            success = ti.success > 0

        results.append(
            {
                "example": example.name,
                "success": int(success),
                "timeout": int(timeout),
                **{param_name: opt_name for param_name, opt_name, _ in run_info},
                **stats,
            }
        )
        logger.info(f"Saved a run result, NITTA runtime - {elapsed_time:.2f}s")


async def _main_in_ctx(results: List[dict], config: EvaluationConfig):
    # this is a list of possible degrees of freedom (its len = number of evaluated parameters)
    search_space_dofs = [
        [(param_name, opt_name, opt_args) for opt_name, opt_args in param_opts.items()]
        for param_name, param_opts in config.evaluated_args.items()
    ]
    # each element of this list is a list of possible setups for a specific evaluated parameter
    # permuntations of these *elements* configure the actual run that will be done later (called "run info" below)
    # "opt" here is a short for "option", "opt_args" correspond to the actual CLI arguments that will be passed to NITTA

    examples_paths = (
        list(EXAMPLES_DIR.glob("**/*.lua"))
        if config.examples == "all"
        else [EXAMPLES_DIR.joinpath(example) for example in config.examples]
    )

    runs = list(
        itertools.product(
            examples_paths, range(config.measurement_tries), *search_space_dofs
        )
    )
    random.shuffle(runs)
    for i, (example, measurement_try, *run_info) in enumerate(runs):
        # run_info: (param_name, opt_name, opt_args)[]

        params_readable_str = ", ".join(
            f"{param_name}={opt_name}" for param_name, opt_name, _ in run_info
        )
        logger.info(
            f">>> >>> >>> Processing {example} ({i+1} / {len(runs)}) >>> {params_readable_str}"
        )

        try:
            await _do_a_run_and_save_results(results, config, run_info, example)
        except Exception:
            logger.exception("Failed to do a run, skipping and continuing")


async def _main(app_args: Namespace, *main_args, **main_kwargs):
    if app_args.with_ml_backend:
        from components.common.ml_backend_running import run_ml_backend

        async with run_ml_backend():
            return await _main_in_ctx(*main_args, **main_kwargs)

    return await _main_in_ctx(*main_args, **main_kwargs)


def _aggregate_and_save_results(results: List[dict], config: EvaluationConfig):
    if not results:
        return

    # dumping raw results just in case something goes wrong while aggregating them
    with open(Path(config.output_dir).joinpath("latest_results.pickle"), "wb") as f:
        pickle.dump((results, config), f)

    key_cols = ["example", *config.evaluated_args.keys()]
    counter_metrics_cols = ["success", "timeout"]
    all_cols: Set[str] = reduce(
        operator.or_, (set(run.keys()) for run in results), set()
    )
    metrics_cols = all_cols - set(key_cols) - set(counter_metrics_cols)

    def keyfunc(run):
        return tuple(run[k] for k in key_cols)

    aggregated_results: List[dict] = []
    results.sort(key=keyfunc)
    for key, runs_group in itertools.groupby(results, keyfunc):
        metric_to_vals = defaultdict(list)
        runs_count = 0
        for run in runs_group:
            runs_count += 1
            for metric, value in run.items():
                if metric not in key_cols:
                    metric_to_vals[metric].append(value)

        Aggregator = Callable[[list], Union[float, int]]

        def _zero_if_shorter_than(threshold: int, func: Aggregator) -> Aggregator:
            return lambda xs: 0 if len(xs) < threshold else func(xs)

        # this dict exists mainly not to confuse mypy
        aggregators: Dict[str, Aggregator] = {
            "sum": _zero_if_shorter_than(1, sum),
            "mean": _zero_if_shorter_than(1, mean),
            "std": _zero_if_shorter_than(2, stdev),
            "min": _zero_if_shorter_than(1, min),
            "max": _zero_if_shorter_than(1, max),
        }

        aggregated_results.append(
            {
                **{param: value for param, value in zip(key_cols, key)},
                "runs": runs_count,
                **{
                    # effectively, rate = sum / runs_count, which is the same as mean of 0/1 values
                    f"{col}_rate": aggregators["mean"](metric_to_vals[col])
                    for col in counter_metrics_cols
                },
                **{
                    f"{col}_{agg}": aggregators[agg](metric_to_vals[col])
                    for col in metrics_cols
                    for agg in ("mean", "std", "min", "max")
                },
            }
        )

    save_dicts_list_to_csv_with_timestamp(
        aggregated_results, config.output_dir, "evaluation", what="evaluation results"
    )


if __name__ == "__main__":
    configure_logging(logging.INFO)

    argparser = _build_argparser()
    args = argparser.parse_args()
    config = _build_config(args.config, args)

    results: List[dict] = []
    success = True

    global_start = datetime.now()

    try:
        asyncio.run(_main(args, results, config))
    except KeyboardInterrupt:
        logger.info("Interrupted by user, saving what we have")
    except Exception:
        logger.exception("Exception occurred, saving what we have")
        success = False
    finally:
        elapsed = str(datetime.now() - global_start).split(".")[0]
        logger.info(
            f"=== Evaluation has been COMPLETED with {len(results)} results in {elapsed} ==="
        )
        _aggregate_and_save_results(results, config)
        sys.exit(0 if success else 1)
