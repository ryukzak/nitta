import asyncio
import os
import pickle
import random
import sys
from asyncio import sleep
from pathlib import Path
from subprocess import Popen
from typing import Tuple, List

import pandas as pd
from joblib import Parallel, delayed

from components.common.utils import is_port_in_use
from components.data_crawling.tree_processing import assemble_tree_dataframe
from components.data_crawling.tree_retrieving import retrieve_whole_nitta_tree
from consts import DATA_DIR, ROOT_DIR

_NITTA_PORT_RANGE: Tuple[int, int] = (33000, 53000)
_NITTA_START_WAIT_DELAY_S: int = 2


async def run_example_and_retrieve_tree_data(example: Path,
                                             data_dir: Path = DATA_DIR,
                                             nitta_exe_path: str = "stack exec nitta -- ") -> pd.DataFrame:
    port: int = random.randint(*_NITTA_PORT_RANGE)
    while is_port_in_use(port):
        print(f"Port {port} is already in use, trying {port + 1}")
        port += 1

    example_name = os.path.basename(example)
    nitta_baseurl = f"http://localhost:{port}"

    print(f"Processing example {example!r}.")
    cmd = f"{nitta_exe_path} -p={port} {example}"
    with Popen(cmd, cwd=str(ROOT_DIR), stdout=sys.stdout, stderr=sys.stderr, shell=True) as proc:
        try:
            print(f"NITTA has been launched, PID {proc.pid}. Waiting for {_NITTA_START_WAIT_DELAY_S} secs.")
            await sleep(_NITTA_START_WAIT_DELAY_S)

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
        finally:
            # TODO: use command-runner library as NITTA is not running in the same process (shell=True)
            #  and doesn't always get killed on certain platforms like Windows.
            proc.terminate()
            print(f"NITTA has been killed")

    print("DONE")
    return df


def get_data_for_many_examples_parallel(examples: List[Path], **runner_kwargs):
    def job(example: Path):
        return asyncio.run(run_example_and_retrieve_tree_data(example, **runner_kwargs))

    Parallel(n_jobs=3)(delayed(job)(example) for example in examples)
