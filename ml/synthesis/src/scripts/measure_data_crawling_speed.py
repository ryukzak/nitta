import asyncio
from pathlib import Path
from time import perf_counter

from components.data_crawling.example_running import run_example_and_retrieve_tree_data

example = Path(r"examples/sum.lua")

start_time = perf_counter()
asyncio.run(run_example_and_retrieve_tree_data(example))
print(f"Finished in {perf_counter() - start_time:.2f} s")
