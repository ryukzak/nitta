import asyncio
import os
import pickle
import random
import signal
import sys
from asyncio import sleep
from contextlib import asynccontextmanager
from pathlib import Path
from typing import AsyncGenerator
from typing import Tuple, List

import pandas as pd
from joblib import Parallel, delayed

from components.common.utils import is_port_in_use
from components.data_crawling.tree_processing import assemble_tree_dataframe
from components.data_crawling.tree_retrieving import retrieve_whole_nitta_tree
from consts import DATA_DIR, ROOT_DIR

_NITTA_PORT_RANGE: Tuple[int, int] = (33000, 53000)
_NITTA_START_WAIT_DELAY_S: int = 2


@asynccontextmanager
async def run_nitta(example: Path,
                    nitta_exe_path: str = "stack exec nitta -- ",
                    port: int = None
                    ) -> AsyncGenerator[Tuple[asyncio.subprocess.Process, str], None]:
    if port is None:
        port = random.randint(*_NITTA_PORT_RANGE)
        while is_port_in_use(port):
            print(f"Port {port} is already in use, trying {port + 1}")
            port += 1

    nitta_baseurl = f"http://localhost:{port}"

    print(f"Processing example {example!r}.")
    cmd = f"{nitta_exe_path} -p={port} {example}"

    try:
        proc = await asyncio.create_subprocess_shell(cmd, cwd=str(ROOT_DIR), stdout=sys.stdout, stderr=sys.stderr, shell=True)

        print(f"NITTA has been launched, PID {proc.pid}. Waiting for {_NITTA_START_WAIT_DELAY_S} secs.")
        await sleep(_NITTA_START_WAIT_DELAY_S)

        yield (proc, nitta_baseurl)
    finally:
        if proc.returncode is None:
            # kill shell and nitta under it
            os.killpg(os.getpgid(proc.pid), signal.SIGTERM)
            await proc.wait()


async def run_example_and_retrieve_tree_data(example: Path,
                                             data_dir: Path = DATA_DIR,
                                             nitta_exe_path: str = "stack exec nitta -- ") -> pd.DataFrame:
    example_name = os.path.basename(example)
    async with run_nitta(example, nitta_exe_path) as (proc, nitta_baseurl):
        print(f"Retrieving tree.")
        tree = await retrieve_whole_nitta_tree(nitta_baseurl)
        data_dir.mkdir(exist_ok=True)

        tree_dump_fn = data_dir / f"{example_name}.pickle"
        print(f"Dumping tree to {tree_dump_fn}.")
        with tree_dump_fn.open("wb") as f:
            pickle.dump(tree, f)

        print(f"Nodes: {tree.subtree_size}. Building dataframe.")
        df = assemble_tree_dataframe(example_name, tree).reset_index(drop=True)

        print(f"Data's ready, {len(df)} rows")

        target_filepath = data_dir / f"{example_name}.csv"
        print(f"Saving to {target_filepath}")
        df.to_csv(target_filepath, index=False)

    print("DONE")
    return df


def get_data_for_many_examples_parallel(examples: List[Path], **runner_kwargs):
    def job(example: Path):
        return asyncio.run(run_example_and_retrieve_tree_data(example, **runner_kwargs))

    Parallel(n_jobs=3)(delayed(job)(example) for example in examples)
