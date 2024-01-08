from __future__ import annotations

import asyncio
import logging
from asyncio import gather
from contextlib import AsyncExitStack
from pathlib import Path

from aiohttp import ClientSession
from joblib import Parallel, delayed
from tqdm.auto import tqdm
from tqdm.std import tqdm as tqdm_instance

from components.common.logging import get_logger
from components.common.nitta_node import NittaNodeInTree
from components.data_crawling.leaf_metrics_collector import LeafMetricsCollector
from components.data_crawling.nitta.nitta_running import NittaRunResult, run_nitta_server
from components.data_crawling.nitta.tree_retrieving import retrieve_random_descending_thread, retrieve_tree_root
from components.data_crawling.tree_sampling.backpropagation import assemble_training_data_via_backpropagation_from_leaf
from components.utils.tqdm_joblib import tqdm_joblib

logger = get_logger(__name__)


DEFAULT_N_SAMPLES = 5000
DEFAULT_N_SAMPLES_PER_BATCH = 150  # was empirically found to yield maximum samples/s
DEFAULT_N_WORKERS = 1
DEFAULT_N_NITTAS = 1
DEFAULT_NITTA_RUN_COMMAND = "stack exec nitta --"


async def run_synthesis_tree_sampling(
    example: Path,
    n_samples: int = DEFAULT_N_SAMPLES,
    n_samples_per_batch=DEFAULT_N_SAMPLES_PER_BATCH,
    n_workers: int = DEFAULT_N_WORKERS,
    n_nittas: int = DEFAULT_N_NITTAS,
    nitta_run_command: str = DEFAULT_NITTA_RUN_COMMAND,
    results_accum: list[dict] | None = None,
) -> list[dict]:
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
    # eariler we used deque here, but that was needed only for O(1) appendleft.
    # now we don't use appendleft anymore, so list should be fine.
    if results_accum is None:
        results_accum = []

    async with AsyncExitStack() as stack:
        nittas: list[NittaRunResult] = await gather(
            *[stack.enter_async_context(run_nitta_server(example, nitta_run_command)) for _ in range(n_nittas)],
        )
        nitta_baseurls = list(await gather(*[nitta.get_base_url() for nitta in nittas]))
        session = await stack.enter_async_context(ClientSession())

        logger.info("Retrieving tree root...")
        root = await retrieve_tree_root(nitta_baseurls[0], session)

        n_batches = n_samples // n_samples_per_batch + 1

        logger.info(
            f"\n\t=== Sampling tree of {example.name}: ==="
            + f"\n\t{n_batches} batches"
            + f"\n\t{n_samples_per_batch} samples per batch"
            + f"\n\t=> {n_samples} total samples"
            + f"\n\tdistributed over {n_workers} worker process(es)"
            + f"\n\twith {n_nittas} NITTA instance(s) running",
        )

        tqdm_args: dict = dict(total=n_samples, desc=f"Sampling {example.name}", unit="samples")

        if n_workers > 1:
            with tqdm_joblib(n_samples_per_batch, **tqdm_args):
                results_accum.extend(
                    sum(
                        Parallel(n_jobs=n_workers)(
                            delayed(_retrieve_and_process_tree_with_sampling_remote_job)(
                                nitta_baseurl=nitta_baseurls[i % len(nitta_baseurls)],
                                root=root,
                                n_samples=n_samples_per_batch,
                                n_samples_per_batch=n_samples_per_batch,
                                example_name=example.name,
                            )
                            for i in range(n_batches)
                        ),
                        [],
                    ),
                )
        else:
            # set logging level to INFO for tqdm (otherwise DEBUG messages will break tqdm's progress bar)
            old_logging_level = logging.getLogger().getEffectiveLevel()
            logging.getLogger().setLevel(logging.INFO)

            with tqdm(**tqdm_args) as pbar:
                await _retrieve_and_process_tree_with_sampling(
                    results_accum=results_accum,
                    session=session,
                    nitta_baseurl=nitta_baseurls[0],
                    root=root,
                    metrics_collector=LeafMetricsCollector(),
                    n_samples=n_samples,
                    n_samples_per_batch=n_samples_per_batch,
                    pbar=pbar,
                    example_name=example.name,
                )

            logging.getLogger().setLevel(old_logging_level)

    return results_accum


async def _retrieve_and_process_single_tree_sample(
    results_accum: list[dict],
    session: ClientSession,
    nitta_baseurl: str,
    root: NittaNodeInTree,
    metrics_collector: LeafMetricsCollector,
    example_name: str | None = None,
):
    leaf = await retrieve_random_descending_thread(root, nitta_baseurl, session, ignore_dirty_tree=True)
    metrics_collector.collect_leaf_node(leaf)
    return assemble_training_data_via_backpropagation_from_leaf(
        results_accum,
        leaf,
        metrics_collector,
        example_name=example_name,
    )


async def _retrieve_and_process_tree_with_sampling(
    results_accum: list[dict],
    session: ClientSession,
    nitta_baseurl: str,
    root: NittaNodeInTree,
    metrics_collector: LeafMetricsCollector,
    n_samples: int,
    n_samples_per_batch: int,
    pbar: tqdm_instance | None = None,
    example_name: str | None = None,
):
    """
    Implements sampling-based tree gathering and processing into training data.

    Forks the sampling into N coroutines-batches and uses `asyncio.gather` to parallelize their IO waits.
    """
    for batch_n in range(n_samples // n_samples_per_batch + 1):
        samples_left = (
            n_samples_per_batch if batch_n < n_samples // n_samples_per_batch else n_samples % n_samples_per_batch
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
            ],
        )
        if pbar is not None:
            pbar.update(samples_left)

    return results_accum


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

    return asyncio.run(_async_job())
