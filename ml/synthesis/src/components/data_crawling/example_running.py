import asyncio
import logging
import os
import pickle
import signal
import sys
from asyncio import sleep
from collections import deque
from contextlib import asynccontextmanager
from pathlib import Path
from typing import AsyncGenerator, Deque, List, Optional, Tuple

import pandas as pd
from aiohttp import ClientSession
from joblib import Parallel, delayed
from tqdm.auto import tqdm
from tqdm_joblib import tqdm_joblib

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


async def run_example_and_sample_tree_parallel(
    example: Path,
    n_samples: int,
    n_workers: int = 4,
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

    # TODO: for now, we start one NITTA per all workers (not per worker). This already gives 100% CPU, but we might want to test it  and change it later.
    """
    example_name = os.path.basename(example)
    async with run_nitta(example, nitta_exe_path) as (_, nitta_baseurl):
        async with ClientSession() as session:
            logger.info(f"Retrieving tree root...")
            root = await retrieve_tree_root(nitta_baseurl, session)

            logger.info(f"Sampling tree ({n_samples} samples, {n_workers} workers)...")

            if n_workers > 1:

                def job(*args):
                    return asyncio.run(_retrieve_and_process_tree_sample_remote(*args))

                with tqdm_joblib(desc=f"Tree sampling", total=n_samples):
                    results = Parallel(n_jobs=n_workers)(
                        delayed(job)(nitta_baseurl, root) for i in range(n_samples)
                    )
            else:
                logging.getLogger().setLevel(logging.INFO)
                results = await _retrieve_and_process_tree_sample(
                    nitta_baseurl, root, session, n_samples
                )
                logging.getLogger().setLevel(logging.DEBUG)

    logger.info(f"DONE: {len(results)} results, {len(set(results))} unique")
    return results


async def _retrieve_and_process_tree_sample(
    nitta_baseurl: str,
    root: NittaNodeInTree,
    session: ClientSession,
    n_samples: int,
    samples_per_batch: int = 150,
):
    """
    A local worker version of _retrieve_and_process_tree_sample.
    It batches the coroutines and uses `asyncio.gather` to parallelize sampling.

    samples_per_batch's default was empirically found to be giving maximum it/s
    """
    results: Deque[str] = deque()

    with tqdm(total=n_samples, desc=f"Tree sampling") as pbar:
        for batch_n in range(n_samples // samples_per_batch + 1):
            samples_left = (
                samples_per_batch
                if batch_n < n_samples // samples_per_batch
                else n_samples % samples_per_batch
            )
            results.append(
                await asyncio.gather(
                    *[
                        _retrieve_and_process_tree_sample_remote(
                            nitta_baseurl,
                            root,
                            keep_tree=True,
                            existing_session=session,
                        )
                        for _ in range(samples_left)
                    ]
                )
            )
            pbar.update(samples_left)

    return sum(results, [])


async def _retrieve_and_process_tree_sample_remote(
    nitta_baseurl: str,
    root: NittaNodeInTree,
    keep_tree: bool = False,
    existing_session: Optional[ClientSession] = None,
):
    """This can be used as a remote worker job, but is handy for local run as well."""
    session = existing_session or ClientSession()
    try:
        if not keep_tree:
            root = (
                root.copy()
            )  # we'll be modifying the tree and we should free related RAM when we're done, so copying

        leaf = await retrieve_random_descending_thread(
            root, nitta_baseurl, session, ignore_dirty_tree=keep_tree
        )

        return leaf.sid
    finally:
        if not existing_session:
            await session.close()
