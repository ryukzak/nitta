"""
A script for evaluation of NITTA's synthesis. It runs NITTA with different arguments, measures metrics (like time,
synthesis success rate, etc.) and writes aggregated results to a single CSV table for further analysis.

!!! Do not add non-stdlib imports here and be compatible with Python 3.8 !!!

By design, this script can be run WITHOUT any non-stdlib dependencies, so it doesn't require installation, just
a compatible Python (3.8+ should be fine). This enables evaluating NITTA without full Python-ML stack installed. So be
careful when adding additional imports here or in modules that this script depends on.
"""
from __future__ import annotations

import asyncio
import itertools
import json
import logging
import pickle
import random
import sys
from argparse import ArgumentParser, BooleanOptionalAction
from collections import defaultdict
from dataclasses import MISSING, dataclass, field, fields
from datetime import datetime
from functools import reduce
from pathlib import Path
from statistics import mean, stdev
from time import perf_counter
from typing import Callable, Iterable, Iterator, Literal, cast
from urllib.request import urlopen

from components.common.logging import configure_logging, get_logger
from components.common.saving import save_dicts_list_to_csv_with_timestamp
from components.data_crawling.nitta.nitta_running import run_nitta_server
from components.utils.string import camel_case_to_snake
from consts import EVALUATIONS_DIR, EXAMPLES_DIR

logger = get_logger(__name__)

# left out of the config since considered not that important
_TIMEOUT_CHECKING_INTERVAL_S = 2


@dataclass
class EvaluationConfig:
    # raw values from the config file / CLI args
    evaluated_args: dict[str, dict[str, str]]
    output_dir: Path = field(default=EVALUATIONS_DIR, metadata=dict(help="Directory to save results to."))
    nitta_run_command: str = field(default="stack exec nitta --", metadata=dict(help="Command to run NITTA."))
    nitta_run_timeout_s: int = field(default=60, metadata=dict(help="Timeout for a single NITTA synthesis run."))
    measurement_tries: int = field(
        default=3,
        metadata=dict(help="How many times to measure each run options combination."),
    )
    examples: list[str] | Literal["all"] = field(default="all", metadata=dict(help="Examples to evaluate on."))
    constant_args: str = field(
        default="-e",
        metadata=dict(help="Additional arguments that are passed to NITTA during all runs."),
    )
    with_ml_backend: bool = field(
        default=True,
        metadata=dict(
            help="Whether to run and keep a single ML backend server for reuse during all NITTA runs. "
            + "If False, NITTA will start/stop a new server for each run.",
        ),
    )


def read_evaluation_config_from_json(config_path: Path, **overrides) -> EvaluationConfig:
    with config_path.open() as f:
        config = json.load(f)

    for arg, value in overrides.items():
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

    # iterate fields in EvaluationConfig and add them to the argparser if type is supported
    _supported_types: dict[str, dict] = {
        "int": dict(type=int),
        "Path": dict(type=Path),
        "str": dict(type=str),
        "bool": dict(action=BooleanOptionalAction),
    }
    for fld in fields(EvaluationConfig):
        if fld.type in _supported_types:
            cli_field_name = fld.name.replace("_", "-")
            default_help = f"Overrides the value of {fld.name!r} from the config file."

            default_in_help = ""
            if fld.default is not MISSING:
                default_in_help = f' Default: "{fld.default}"'

            argparser.add_argument(
                f"--{cli_field_name}",
                help=fld.metadata.get("help", default_help) + default_in_help,
                metavar=fld.type.upper(),
                required=False,
                **_supported_types.get(cast(str, fld.type), {}),
            )

    return argparser


@dataclass
class _NittaTreeInfo:
    nodes_visited: int
    nodes_success: int
    nodes_failed: int
    nodes_not_processed: int
    target_process_duration: dict[str, int]
    synthesis_steps_for_success: dict[str, int]


def _get_tree_info_from_nitta(nitta_base_url: str) -> _NittaTreeInfo:
    with urlopen(nitta_base_url + "/treeInfo") as resp:
        ti_dict = json.loads(resp.read().decode("utf8"))

    return _NittaTreeInfo(**{camel_case_to_snake(k): v for k, v in ti_dict.items()})


async def _assemble_stats_dict_after_synthesis(nitta_base_url: str, elapsed_time: float) -> tuple[_NittaTreeInfo, dict]:
    ti = _get_tree_info_from_nitta(nitta_base_url)

    logger.info(f"Got tree info: {ti}")

    if not ti.nodes_success > 0:
        logger.info("Synthesis was not successful, skipping stats collection")
        return ti, {}

    def _mean_from_tree_info_dict(d: dict) -> float:
        # dicts are like {value: node_count}, we need a weighted mean
        # {"13": 3, "9": 6} -> (3*13 + 6*9) / (3+6)
        if sum(d.values()) == 0:
            return 0
        return sum(int(k) * v for k, v in d.items()) / sum(d.values())

    return ti, {
        "time": elapsed_time,
        "synthesis_steps": ti.nodes_visited - ti.nodes_not_processed - 1,  # -1 for root
        "mean_depth": _mean_from_tree_info_dict(ti.synthesis_steps_for_success),
        "min_duration": min(int(k) for k in ti.target_process_duration.keys()),
        "leafs": ti.nodes_success + ti.nodes_failed,
        "leaf_success_rate": ti.nodes_success / (ti.nodes_success + ti.nodes_failed),
        "nodes_total": ti.nodes_visited,
        "nodes_not_processed": ti.nodes_not_processed,
    }


async def _do_a_run_and_save_results(
    results: list[dict],
    config: EvaluationConfig,
    run_info: Iterable[tuple[str, str, str]],
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
                nitta_base_url = await asyncio.wait_for(nitta.get_base_url(), timeout=_TIMEOUT_CHECKING_INTERVAL_S)
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
            logger.info(f"Detected a NITTA API server on {nitta_base_url}. Getting tree info...")
            ti, stats = await _assemble_stats_dict_after_synthesis(nitta_base_url, elapsed_time)
            success = ti.nodes_success > 0

        results.append(
            {
                "example": example.name,
                "success": int(success),
                "timeout": int(timeout),
                **{param_name: opt_name for param_name, opt_name, _ in run_info},
                **stats,
            },
        )
        logger.info(f"Saved a run result, NITTA runtime - {elapsed_time:.2f}s")


async def _do_the_runs(results: list[dict], config: EvaluationConfig):
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

    runs = list(itertools.product(examples_paths, range(config.measurement_tries), *search_space_dofs))
    random.shuffle(runs)
    for i, (example, measurement_try, *run_info) in enumerate(runs):
        # run_info: (param_name, opt_name, opt_args)[]
        del measurement_try  # unused

        params_readable_str = ", ".join(f"{param_name}={opt_name}" for param_name, opt_name, _ in run_info)
        logger.info(f">>> >>> >>> Processing {example} ({i+1} / {len(runs)}) >>> {params_readable_str}")

        try:
            await _do_a_run_and_save_results(results, config, run_info, example)
        except Exception:
            logger.exception("Failed to do a run, skipping and continuing")

    return len(runs)


async def _prepare_and_do_the_runs(*, config: EvaluationConfig, **other_kwargs):
    if config.with_ml_backend:
        from components.common.ml_backend_running import run_ml_backend

        async with run_ml_backend():
            return await _do_the_runs(config=config, **other_kwargs)

    return await _do_the_runs(config=config, **other_kwargs)


def _select_keys_from_longest_dict(list_of_dicts: list[dict]) -> list[str]:
    def _reducer(accum: list[str], next_: dict):
        if len(next_) > len(accum):
            accum = list(next_.keys())
        return accum

    return reduce(_reducer, list_of_dicts, [])


def _extract_values_of_metrics_and_runs_count(runs_group: Iterator[dict], key_cols: list[str]):
    metric_to_vals = defaultdict(list)
    runs_count = 0
    for run in runs_group:
        runs_count += 1
        for metric, value in run.items():
            if metric not in key_cols:
                metric_to_vals[metric].append(value)
    return runs_count, metric_to_vals


_Aggregator = Callable[[list], (float | None)]


def _aggregate_and_save_results(results: list[dict], config: EvaluationConfig):
    if not results:
        return

    # dumping raw results just in case something goes wrong while aggregating them
    pickle_output = Path(config.output_dir) / "latest_results.pickle"
    with pickle_output.open("wb") as f:
        pickle.dump((results, config), f)

    key_cols = ["example", *config.evaluated_args.keys()]
    counter_metrics_cols = ["success", "timeout"]

    all_cols = _select_keys_from_longest_dict(results)
    metrics_cols = [col for col in all_cols if col not in key_cols and col not in counter_metrics_cols]

    def _sorting_key(run):
        return tuple(run[k] for k in key_cols)

    aggregated_results: list[dict] = []
    results.sort(key=_sorting_key)
    for key, runs_group in itertools.groupby(results, key=_sorting_key):
        runs_count, metric_to_vals = _extract_values_of_metrics_and_runs_count(runs_group, key_cols)

        def _none_if_shorter_than(threshold: int, func: _Aggregator) -> _Aggregator:
            return lambda xs: None if len(xs) < threshold else func(xs)

        # this dict exists mainly not to confuse mypy
        aggregators: dict[str, _Aggregator] = {
            "sum": _none_if_shorter_than(1, sum),
            "mean": _none_if_shorter_than(1, mean),
            "std": _none_if_shorter_than(2, stdev),
            "min": _none_if_shorter_than(1, min),
            "max": _none_if_shorter_than(1, max),
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
            },
        )

    save_dicts_list_to_csv_with_timestamp(
        aggregated_results,
        config.output_dir,
        "evaluation",
        what="evaluation results",
    )


def evaluate_nitta_synthesis(config: EvaluationConfig):
    results: list[dict] = []
    success = True

    global_start = datetime.now()

    total_runs = None
    try:
        total_runs = asyncio.run(_prepare_and_do_the_runs(results=results, config=config))
    except KeyboardInterrupt:
        logger.info("Interrupted by user, saving what we have")
    except Exception:
        logger.exception("Exception occurred, saving what we have")
        success = False
    finally:
        elapsed = str(datetime.now() - global_start).split(".")[0]
        logger.info(f"=== Evaluation has been COMPLETED with {len(results)} results in {elapsed} ===")

        if total_runs is not None:
            runs_with_errors = total_runs - len(results)
            if runs_with_errors > 0:
                logger.info(f"Total runs count: {total_runs}, runs with errors: {runs_with_errors}")

        _aggregate_and_save_results(results, config)

        return success


if __name__ == "__main__":
    configure_logging(logging.INFO)

    argparser = _build_argparser()
    args = argparser.parse_args()
    config = read_evaluation_config_from_json(args.config, **vars(args))

    success = evaluate_nitta_synthesis(config)

    sys.exit(0 if success else 1)
