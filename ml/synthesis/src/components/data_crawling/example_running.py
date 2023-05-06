import asyncio
import logging
import os
import pickle
import signal
import sys
from asyncio import gather, sleep
from collections import deque
from contextlib import AsyncExitStack, ExitStack, asynccontextmanager
from pathlib import Path
from typing import AsyncGenerator, Deque, List, Optional, Tuple

import pandas as pd
from aiohttp import ClientSession
from joblib import Parallel, delayed
from tqdm.auto import tqdm
from tqdm.std import tqdm as tqdm_instance

from components.common.logging import get_logger
from components.common.nitta_node import NittaNodeInTree
from components.common.port_management import find_random_free_port
from components.data_crawling.node_processing import get_subtree_size
from components.data_crawling.tree_processing import assemble_tree_dataframe
from components.data_crawling.tree_retrieving import (
    retrieve_random_descending_thread,
    retrieve_tree_root,
    retrieve_whole_nitta_tree,
)
from components.utils.tqdm_joblib import tqdm_joblib
from consts import DATA_DIR, ROOT_DIR

logger = get_logger(__name__)

_NITTA_START_WAIT_DELAY_S: int = 2


@asynccontextmanager
async def run_nitta(
    example: Path,
    nitta_exe_path: str = "stack exec nitta -- ",
    nitta_args: str = "",
    nitta_env: Optional[dict] = None,
    port: Optional[int] = None,
) -> AsyncGenerator[Tuple[asyncio.subprocess.Process, str], None]:
    if port is None:
        port = find_random_free_port()

    nitta_baseurl = f"http://localhost:{port}"

    logger.info(f"Processing example {example!r}.")
    cmd = f"{nitta_exe_path} -p={port} {nitta_args} {example}"

    env = os.environ.copy()
    env.update(nitta_env or {})

    proc = None
    try:
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
    Instead of copying the whole tree to Python and only then processing it, we sample the tree (randomly descend many
    times) and process samples on the fly. This way we:
    - can parallelize processing of a single example tree, which was troublesome before
    - can handle huge trees (astronomical number of nodes)  <--- this is the main reason

    This way we also don't need to store the whole tree in Python RAM, but caching tree node info in RAM speeds things
    up significantly, so trees should be kept in RAM when possible. Reasonable default was implemented: they are kept
    in RAM, but freed when RAM becomes low.

    We sacrifice accuracy, but it seems to be a reasonable trade-off.

    About n_nittas: for Intel Core i5-12400F (6 cores, 12 threads)
    - 1 NITTA is enough to give 80%-100% load on all cores, seems optimal
    - 2+ NITTAs lead to huge performance drop (context switching, perhaps)
    So, n_nittas=1 seems the best choice for now.

    About n_workers: we can parallelize on IO waits thanks to asyncio. It seems enough, because multiprocessing
    doesn't give any performance boost (perhaps, even the opposite due to the overhead). So, n_workers=1 fits best too.
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

        if n_workers > 1:

            def job(**kwargs):
                return asyncio.run(_retrieve_and_process_tree_with_sampling(**kwargs))

            with tqdm_joblib(samples_per_batch, **tqdm_args):
                results = Parallel(n_jobs=n_workers)(
                    delayed(job)(
                        nitta_baseurl=nitta_baseurls[i % len(nitta_baseurls)],
                        root=root,
                        n_samples=samples_per_batch,
                        samples_per_batch=samples_per_batch,
                    )
                    for i in range(n_batches)
                )
        else:
            logging.getLogger().setLevel(logging.INFO)
            with tqdm(**tqdm_args) as pbar:
                results = await _retrieve_and_process_tree_with_sampling(
                    nitta_baseurls[0],
                    root,
                    n_samples,
                    samples_per_batch=samples_per_batch,
                    existing_session=session,
                    pbar=pbar,
                )
            logging.getLogger().setLevel(logging.DEBUG)

    logger.info(f"DONE: {len(results)} results, {len(set(results))} unique")
    return results


async def _retrieve_and_process_tree_with_sampling(
    nitta_baseurl: str,
    root: NittaNodeInTree,
    n_samples: int,
    samples_per_batch: int = _DEFAULT_SAMPLES_PER_BATCH,
    existing_session: Optional[ClientSession] = None,
    pbar: Optional[tqdm_instance] = None,
):
    """
    A job for local or remote worker that implements sampling-based tree gathering and processing into training data.

    Forks the sampling into N coroutines-batches and uses `asyncio.gather` to parallelize their IO waits.
    """
    session = existing_session or ClientSession()
    try:
        results: Deque[str] = deque()

        for batch_n in range(n_samples // samples_per_batch + 1):
            samples_left = (
                samples_per_batch
                if batch_n < n_samples // samples_per_batch
                else n_samples % samples_per_batch
            )
            results.append(
                await asyncio.gather(
                    *[
                        _retrieve_and_process_single_tree_sample(
                            nitta_baseurl,
                            root,
                            session,
                        )
                        for _ in range(samples_left)
                    ]
                )
            )
            if pbar is not None:
                pbar.update(samples_left)

        return sum(results, [])
    finally:
        if not existing_session:
            await session.close()


async def _retrieve_and_process_single_tree_sample(
    nitta_baseurl: str,
    root: NittaNodeInTree,
    session: ClientSession,
):
    leaf = await retrieve_random_descending_thread(
        root, nitta_baseurl, session, ignore_dirty_tree=True
    )

    return leaf.sid
