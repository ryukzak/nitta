import asyncio
import logging
import os
import pickle
import re
from asyncio import gather
from contextlib import AsyncExitStack
from pathlib import Path
from typing import List, Optional

import pandas as pd
from aiohttp import ClientSession
from joblib import Parallel, delayed
from tqdm.auto import tqdm
from tqdm.std import tqdm as tqdm_instance

from components.common.logging import get_logger
from components.common.nitta_node import NittaNodeInTree
from components.data_crawling.leaf_metrics_collector import LeafMetricsCollector
from components.data_crawling.nitta_running import run_nitta
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
from consts import DATA_DIR

logger = get_logger(__name__)


async def run_example_and_retrieve_tree_data(
    example: Path,
    data_dir: Path = DATA_DIR,
    nitta_run_command: str = "stack exec nitta -- ",
) -> pd.DataFrame:
    example_name = os.path.basename(example)
    async with run_nitta(example, nitta_run_command) as (_, nitta_baseurl):
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
    n_samples_per_batch=150,
    n_workers: int = 1,
    n_nittas: int = 1,
    nitta_run_command: str = "stack exec nitta -- ",
    results_accum: Optional[List[dict]] = None,
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
                stack.enter_async_context(run_nitta(example, nitta_run_command))
                for _ in range(n_nittas)
            ]
        )
        nitta_baseurls = [nitta_baseurl for (_, nitta_baseurl) in nittas]
        session = await stack.enter_async_context(ClientSession())

        logger.info(f"Retrieving tree root...")
        root = await retrieve_tree_root(nitta_baseurls[0], session)

        n_batches = n_samples // n_samples_per_batch + 1

        logger.info(
            f"Sampling tree ({n_batches} batches * {n_samples_per_batch} = {n_samples} samples >> {n_workers} workers)..."
        )

        tqdm_args = dict(total=n_samples, desc=f"Tree sampling", unit="samples")

        # we needed deque only for O(1) appendleft. here we don't use appendleft, so list should be fine.
        if results_accum is None:
            results_accum = []

        if n_workers > 1:
            with tqdm_joblib(n_samples_per_batch, **tqdm_args):
                results_accum.extend(
                    sum(
                        Parallel(n_jobs=n_workers)(
                            delayed(
                                _retrieve_and_process_tree_with_sampling_remote_job
                            )(
                                nitta_baseurl=nitta_baseurls[i % len(nitta_baseurls)],
                                root=root,
                                n_samples=n_samples_per_batch,
                                n_samples_per_batch=n_samples_per_batch,
                                example_name=example_name,
                            )
                            for i in range(n_batches)
                        ),
                        [],
                    )
                )
        else:
            # set logging level to INFO for tqdm (otherwise DEBUG messages will break tqdm's progress bar)
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
                    example_name=example_name,
                )

            logging.getLogger().setLevel(logging.DEBUG)

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


async def _retrieve_and_process_tree_with_sampling(
    results_accum: List[dict],
    session: ClientSession,
    nitta_baseurl: str,
    root: NittaNodeInTree,
    metrics_collector: LeafMetricsCollector,
    n_samples: int,
    n_samples_per_batch: int = _DEFAULT_SAMPLES_PER_BATCH,
    pbar: Optional[tqdm_instance] = None,
    example_name: Optional[str] = None,
):
    """
    Implements sampling-based tree gathering and processing into training data.

    Forks the sampling into N coroutines-batches and uses `asyncio.gather` to parallelize their IO waits.
    """
    for batch_n in range(n_samples // n_samples_per_batch + 1):
        samples_left = (
            n_samples_per_batch
            if batch_n < n_samples // n_samples_per_batch
            else n_samples % n_samples_per_batch
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
