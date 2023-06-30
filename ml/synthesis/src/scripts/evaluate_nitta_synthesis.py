import asyncio
import itertools
import json
import logging
import random
import sys
from argparse import ArgumentParser
from collections import defaultdict
from dataclasses import dataclass, fields
from pathlib import Path
from statistics import mean, stdev
from time import perf_counter
from typing import Callable, Dict, Iterable, List, Literal, Tuple, Union

from aiohttp import ClientSession

from components.common.logging import configure_logging, get_logger
from components.common.nitta_node import NittaTreeInfo
from components.common.saving import save_dicts_list_to_csv_with_timestamp
from components.data_crawling.nitta_running import run_nitta_server
from components.data_crawling.tree_retrieving import retrieve_tree_info
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


async def _assemble_stats_dict_after_synthesis(
    nitta_base_url: str, elapsed_time: float
) -> Tuple[dict, NittaTreeInfo]:
    stats = {}

    async with ClientSession() as session:
        ti = await retrieve_tree_info(nitta_base_url, session)

    logger.info(f"Got tree info: {ti}")

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
    stats["leaf_success_rate"] = ti.success / stats["leafs"]
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
        elif nitta_base_url is None:
            logger.info(
                f"A error occurred at {elapsed_time:.2f}s, so it's neither a timeout, nor a success."
            )
        else:
            logger.info(
                f"Synthesis done, NITTA API server started on {nitta_base_url}. "
                f"Elapsed time: {elapsed_time:.2f}s. Getting tree info..."
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


async def _main(results: List[dict], config: EvaluationConfig):
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
            elapsed_time = results[-1].get("time")
            if elapsed_time is not None:
                logger.info(f"Finished in {elapsed_time:.2f}s")
        except Exception:
            logger.exception("Failed to do a run, skipping and continuing")


def _aggregate_and_save_results(results: List[dict], config: EvaluationConfig):
    key_cols = ["example", *config.evaluated_args.keys()]
    counter_metrics_cols = ["success", "timeout"]
    metrics_cols = (
        [col for col in results[0].keys() if col not in key_cols + counter_metrics_cols]
        if results
        else []
    )

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

        # this dict exists mainly not to confuse mypy
        aggregators: Dict[str, Callable[[list], Union[float, int]]] = {
            "sum": sum,
            "mean": mean,
            "std": lambda xs: stdev(xs) if len(xs) > 1 else 0,
            "min": min,
            "max": max,
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

    try:
        asyncio.run(_main(results, config))
    except KeyboardInterrupt:
        logger.info("Interrupted by user, saving what we have")
    except Exception:
        logger.exception("Exception occurred, saving what we have")
        success = False
    finally:
        logger.info(f"Finished with {len(results)} results")
        if results:
            _aggregate_and_save_results(results, config)
        sys.exit(0 if success else 1)
