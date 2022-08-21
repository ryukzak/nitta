import asyncio
import sys
from pathlib import Path
from time import perf_counter

from components.data_crawling.example_running import run_example_and_retrieve_tree_data

if __name__ == '__main__':
    example = Path(r"examples/sum.lua")
    if len(sys.argv) == 2:
        example = Path(sys.argv[1])
    else:
        print("Default algorithm:", example)
    start_time = perf_counter()
    asyncio.run(run_example_and_retrieve_tree_data(example))
    print(f"Finished in {perf_counter() - start_time:.2f} s")
