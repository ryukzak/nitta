import asyncio
import logging
import os
import pickle
import signal
import sys
from asyncio import gather, sleep
from contextlib import AsyncExitStack, asynccontextmanager
from datetime import datetime
from math import exp, log
from pathlib import Path
from typing import AsyncGenerator, List, Optional, Tuple

import pandas as pd
from aiohttp import ClientSession
from joblib import Parallel, delayed
from tqdm.auto import tqdm
from tqdm.std import tqdm as tqdm_instance

from components.common.logging import get_logger
from components.common.nitta_node import NittaNodeInTree
from components.common.port_management import find_random_free_port
from components.data_crawling.leaf_metrics_collector import LeafMetricsCollector
from components.data_crawling.node_label_computation import aggregate_node_labels
from components.data_crawling.node_processing import get_subtree_size
from components.data_crawling.tree_processing import (
    assemble_training_data_via_backpropagation_from_leaf,
    assemble_tree_dataframe,
)
from components.data_crawling.tree_retrieving import (
    retrieve_random_descending_thread,
    retrieve_tree_root,
    retrieve_whole_nitta_tree,
)
from components.utils.tqdm_joblib import tqdm_joblib
from consts import DATA_DIR, ROOT_DIR

logger = get_logger(__name__)

_NITTA_START_WAIT_DELAY_S: int = 2
_NITTA_START_RETRY_DELAY_S: int = 2
_NITTA_START_MAX_RETRIES: int = 5


@asynccontextmanager
async def run_nitta(
    example: Path,
    nitta_exe_path: str = "stack exec nitta --",
    nitta_args: str = "",
    nitta_env: Optional[dict] = None,
    given_port: Optional[int] = None,
) -> AsyncGenerator[Tuple[asyncio.subprocess.Process, str], None]:
    env = os.environ.copy()
    env.update(nitta_env or {})

    proc = None
    retries_left = _NITTA_START_MAX_RETRIES
    try:
        while (proc is None or proc.returncode is not None) and retries_left > 0:
            port = given_port or find_random_free_port()

            cmd = f"{nitta_exe_path} -p={port} {nitta_args} {example}"
            logger.info(f"Starting NITTA, cmd: {cmd}")

            preexec_fn = (
                None if os.name == "nt" else os.setsid
            )  # see https://stackoverflow.com/a/4791612
            proc = await asyncio.create_subprocess_shell(
                cmd,
                cwd=str(ROOT_DIR),
                stdout=sys.stdout,
                stderr=sys.stderr,
                shell=True,
                preexec_fn=preexec_fn,
                env=env,
            )

            logger.info(
                f"NITTA has been launched, PID {proc.pid}. Waiting for {_NITTA_START_WAIT_DELAY_S} secs."
            )
            await sleep(_NITTA_START_WAIT_DELAY_S)

            if proc.returncode is not None:
                logger.warning(
                    f"Failed to start NITTA (exit code {proc.returncode}). "
                    + f"Retrying after a delay of {_NITTA_START_RETRY_DELAY_S} secs (retries left: {retries_left})."
                )
                await sleep(_NITTA_START_RETRY_DELAY_S)
                retries_left -= 1
                proc = None

        if proc is None or proc.returncode is not None:
            raise RuntimeError(
                f"Failed to start NITTA after {_NITTA_START_MAX_RETRIES} retries."
            )

        nitta_baseurl = f"http://localhost:{port}"
        yield proc, nitta_baseurl
    finally:
        if proc is not None and proc.returncode is None:
            pid = proc.pid
            pgid = os.getpgid(pid)
            logger.info(f"Killing shell and NITTA under it, PID {pid}, PGID {pgid}")
            os.killpg(pgid, signal.SIGTERM)
            await proc.wait()


async def run_example_and_retrieve_tree_data(
    example: Path,
    data_dir: Path = DATA_DIR,
    nitta_exe_path: str = "stack exec nitta -- ",
) -> pd.DataFrame:
    example_name = os.path.basename(example)
    async with run_nitta(example, nitta_exe_path) as (_, nitta_baseurl):
        logger.info(f"Retrieving tree.")
        tree = await retrieve_whole_nitta_tree(nitta_baseurl)
        data_dir.mkdir(exist_ok=True)

        tree_dump_fn = data_dir / f"{example_name}.pickle"
        logger.info(f"Dumping tree to {tree_dump_fn}.")
        with tree_dump_fn.open("wb") as f:
            pickle.dump(tree, f)

        logger.info(f"Nodes: {get_subtree_size(tree)}. Building dataframe.")
        df = assemble_tree_dataframe(example_name, tree).reset_index(drop=True)

        logger.info(f"Data's ready, {len(df)} rows")

        target_filepath = data_dir / f"{example_name}.csv"
        logger.info(f"Saving to {target_filepath}")
        df.to_csv(target_filepath, index=False)

    logger.info("DONE")
    return df


def get_data_for_many_examples_parallel(examples: List[Path], **runner_kwargs):
    def job(example: Path):
        return asyncio.run(run_example_and_retrieve_tree_data(example, **runner_kwargs))

    Parallel(n_jobs=3)(delayed(job)(example) for example in examples)


_DEFAULT_SAMPLES_PER_BATCH = 150  # was empirically found to yield maximum it/s


async def run_example_and_sample_tree_parallel(
    example: Path,
    n_samples: int,
    n_workers: int = 1,
    n_nittas: int = 1,
    data_dir: Path = DATA_DIR,
    nitta_exe_path: str = "stack exec nitta -- ",
):
    """
    Instead of evaluating the whole tree and only then processing it, we sample the tree (randomly descend many times)
    and process samples on the fly. This way we:
    - can parallelize processing of a single example tree, which was troublesome before
    - can handle huge trees (astronomical number of nodes)  <--- this is the main reason

    This way we also don't need to store the whole tree in RAM, but caching tree node info in RAM speeds things up
    significantly, so trees should be kept in RAM when possible.Major part of RAM is used by NITTA, not Python, so
    for now it's enough to restart the NITTAs (by restarting the sampling process).

    We sacrifice some accuracy, but it seems to be a reasonable trade-off.

    About n_nittas: for Intel Core i5-12400F (6 cores, 12 threads)
    - 1 NITTA is enough to give 80%-100% load on all cores, seems optimal
    - 2+ NITTAs lead to huge performance drop (context switching, perhaps)
    So, n_nittas=1 seems the best choice for now.

    About n_workers: we can parallelize on IO waits thanks to asyncio. It seems enough, because multiprocessing doesn't
    give any performance boost (perhaps, even the opposite due to the overhead). So, n_workers=1 fits best too.
    """
    example_name = os.path.basename(example)
    async with AsyncExitStack() as stack:
        nittas = await gather(
            *[
                stack.enter_async_context(run_nitta(example, nitta_exe_path))
                for _ in range(n_nittas)
            ]
        )
        nitta_baseurls = [nitta_baseurl for (_, nitta_baseurl) in nittas]
        session = await stack.enter_async_context(ClientSession())

        logger.info(f"Retrieving tree root...")
        root = await retrieve_tree_root(nitta_baseurls[0], session)

        samples_per_batch = 150
        n_batches = n_samples // samples_per_batch + 1

        logger.info(
            f"Sampling tree ({n_batches} batches * {samples_per_batch} = {n_samples} samples >> {n_workers} workers)..."
        )

        tqdm_args = dict(total=n_samples, desc=f"Tree sampling", unit="samples")

        # we needed deque only for O(1) appendleft. here we don't use appendleft, so list should be fine.
        results: List[dict] = []

        if n_workers > 1:
            with tqdm_joblib(samples_per_batch, **tqdm_args):
                results = sum(
                    Parallel(n_jobs=n_workers)(
                        delayed(_retrieve_and_process_tree_with_sampling_remote_job)(
                            nitta_baseurl=nitta_baseurls[i % len(nitta_baseurls)],
                            root=root,
                            n_samples=samples_per_batch,
                            samples_per_batch=samples_per_batch,
                            example_name=example_name,
                        )
                        for i in range(n_batches)
                    ),
                    [],
                )
        else:
            logging.getLogger().setLevel(logging.INFO)
            with tqdm(**tqdm_args) as pbar:
                await _retrieve_and_process_tree_with_sampling(
                    results_accum=results,
                    session=session,
                    nitta_baseurl=nitta_baseurls[0],
                    root=root,
                    metrics_collector=LeafMetricsCollector(),
                    n_samples=n_samples,
                    samples_per_batch=samples_per_batch,
                    pbar=pbar,
                    example_name=example_name,
                )
            logging.getLogger().setLevel(logging.DEBUG)

    results_df = _build_df_and_save_sampling_results(results, example_name, data_dir)

    n_results = len(results)
    n_unique_sids = len(results_df.index.unique())  # already deduplicated
    clash_ratio = n_results / n_unique_sids - 1
    tree_cov = _estimate_tree_coverage_based_on_clash_ratio(clash_ratio, n_unique_sids)
    tree_cov_str = f"{tree_cov * 100:.3f}%" if tree_cov else "unknown :("
    logger.info(
        f"TREE SAMPLING DONE: {n_unique_sids} unique nodes, clash ratio: {clash_ratio * 100:.3f}%, "
        + f"estimated tree coverage: {tree_cov_str}"
    )
    return n_unique_sids, clash_ratio


def _retrieve_and_process_tree_with_sampling_remote_job(**kwargs):
    """
    A remote worker process job that does necessary initialization before calling
    `_retrieve_and_process_tree_with_sampling`.
    """

    async def _async_job():
        async with ClientSession() as session:
            return await _retrieve_and_process_tree_with_sampling(
                **kwargs,
                session=session,
                metrics_collector=LeafMetricsCollector(),
                results_accum=[],
            )

    asyncio.run(_async_job())


async def _retrieve_and_process_tree_with_sampling(
    results_accum: List[dict],
    session: ClientSession,
    nitta_baseurl: str,
    root: NittaNodeInTree,
    metrics_collector: LeafMetricsCollector,
    n_samples: int,
    samples_per_batch: int = _DEFAULT_SAMPLES_PER_BATCH,
    pbar: Optional[tqdm_instance] = None,
    example_name: Optional[str] = None,
):
    """
    Implements sampling-based tree gathering and processing into training data.

    Forks the sampling into N coroutines-batches and uses `asyncio.gather` to parallelize their IO waits.
    """
    for batch_n in range(n_samples // samples_per_batch + 1):
        samples_left = (
            samples_per_batch
            if batch_n < n_samples // samples_per_batch
            else n_samples % samples_per_batch
        )
        await asyncio.gather(
            *[
                _retrieve_and_process_single_tree_sample(
                    results_accum,
                    session,
                    nitta_baseurl,
                    root,
                    metrics_collector,
                    example_name,
                )
                for _ in range(samples_left)
            ]
        )
        if pbar is not None:
            pbar.update(samples_left)

    return results_accum


async def _retrieve_and_process_single_tree_sample(
    results_accum: List[dict],
    session: ClientSession,
    nitta_baseurl: str,
    root: NittaNodeInTree,
    metrics_collector: LeafMetricsCollector,
    example_name: Optional[str] = None,
):
    leaf = await retrieve_random_descending_thread(
        root, nitta_baseurl, session, ignore_dirty_tree=True
    )
    metrics_collector.collect_leaf_node(leaf)
    return assemble_training_data_via_backpropagation_from_leaf(
        results_accum, leaf, metrics_collector, example_name=example_name
    )


def _build_df_and_save_sampling_results(
    results: List[dict], example_name: str, data_dir: Path
) -> pd.DataFrame:
    """
    Builds a DataFrame from the sampling results and saves it to the `data_dir`.
    """
    logger.info(
        f"Building a DataFrame from sampling results ({len(results)} entries)..."
    )

    results_df = pd.DataFrame(results)
    sampling_run_id = datetime.now().strftime("%y%m%d_%H%M%S")

    # there's a significant duplication of data in results at this point.
    # clashing nodes differ only in labels.
    # any way to improve this? seems extremely difficult (parallel executions won't be independent)

    results_df = results_df.set_index("sid", drop=True)
    results_df_labels = results_df.groupby(results_df.index).label.agg(
        aggregate_node_labels
    )

    results_df = results_df[~results_df.index.duplicated(keep="first")]
    results_df.label = results_df_labels

    output_fn = data_dir / f"{example_name}.sampling.{sampling_run_id}.csv"
    results_df.to_csv(output_fn)

    logger.info(f"Results saved to {output_fn}")

    return results_df


def _estimate_tree_coverage_based_on_clash_ratio(
    clash_ratio: float, n_nodes: int
) -> Optional[float]:
    """
    This is a bit sketchy yet state-of-the-art ad hoc heuristic estimation of tree coverage based on clash ratio.

    It's based the following hypotheses:
        1) tree coverage is in pure functional (mathematical) dependency on clash ratio
        2) this dependency is tree-independent and is explained purely by random descent algorithm definition,
           probability theory and statistics.

    All approximation functions and coefficients are derived empirically from a set of sampling runs.
    All formulas are arbitrarily chosen to mathematically describe empirical data as close as my math skills allow.

    Error is estimated to be up to 10-15%, but it may depend on the tree peculiarities.

    UPD: found not suitable for large trees (leading to very small cron value) :(
    More data and runs needed, but I'm out of time.
    """
    # clash ratio in percents with a small epsilon to avoid division by zero
    cr = clash_ratio * 100
    n = n_nodes  # number of unique nodes sampled from the tree
    cron = cr / n  # short for "cr over n". like cr, but independent of n.

    if cron < 0.02:
        return None

    # nr - tree coverage, i.e. number of nodes sampled from the tree in this run

    # goal - find "approx(cron*nr)", i.e. approximate value of cron*nr
    # empirical data shows that approximation function depends on cr <> 100% heavily

    # approximation function for cr < 100%, found empirically
    approx_lt100 = -0.019 * log(cron) - 0.0394

    # approximation function for cr > 100%, found empirically
    approx_gt100 = 1.0225 * cron - 0.0288

    # we will blend these two functions with a sigmoid
    blender_width_coef = 0.02  # found empirically
    blender_center = 100.0
    blender_value = 1 / (1 + exp(-blender_width_coef * (cr - blender_center)))

    # blend the two approximation functions (as sigmoid goes 0..1, we go lt100..gt100)
    approx = approx_lt100 * (1 - blender_value) + approx_gt100 * blender_value

    # approx = cron * nr
    approx_nr = max(0, min(1, approx / cron))

    return approx_nr
