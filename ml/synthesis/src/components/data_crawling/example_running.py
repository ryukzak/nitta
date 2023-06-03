import asyncio
import os
import pickle
import signal
import sys
from asyncio import sleep
from contextlib import asynccontextmanager
from pathlib import Path
from typing import AsyncGenerator, List, Tuple

import pandas as pd
from components.common.logging import get_logger
from components.common.port_management import find_random_free_port
from components.data_crawling.tree_processing import assemble_tree_dataframe
from components.data_crawling.tree_retrieving import retrieve_whole_nitta_tree
from consts import DATA_DIR, ROOT_DIR
from joblib import Parallel, delayed

logger = get_logger(__name__)

_NITTA_START_WAIT_DELAY_S: int = 2


@asynccontextmanager
async def run_nitta(
    example: Path,
    nitta_exe_path: str = "stack exec nitta -- ",
    nitta_args: str = "",
    nitta_env: dict = None,
    port: int = None,
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
        preexec_fn = None if os.name == "nt" else os.setsid  # see https://stackoverflow.com/a/4791612
        proc = await asyncio.create_subprocess_shell(
            cmd,
            cwd=str(ROOT_DIR),
            stdout=sys.stdout,
            stderr=sys.stderr,
            shell=True,
            preexec_fn=preexec_fn,
            env=env,
        )

        logger.info(f"NITTA has been launched, PID {proc.pid}. Waiting for {_NITTA_START_WAIT_DELAY_S} secs.")
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
    async with run_nitta(example, nitta_exe_path) as (proc, nitta_baseurl):
        logger.info("Retrieving tree.")
        tree = await retrieve_whole_nitta_tree(nitta_baseurl)
        data_dir.mkdir(exist_ok=True)

        tree_dump_fn = data_dir / f"{example_name}.pickle"
        logger.info(f"Dumping tree to {tree_dump_fn}.")
        with tree_dump_fn.open("wb") as f:
            pickle.dump(tree, f)

        logger.info(f"Nodes: {tree.subtree_size}. Building dataframe.")
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
