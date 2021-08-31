import asyncio

from modules.data_crawling import process_example

example = r"examples/counter.lua"

loop = asyncio.get_event_loop()
loop.run_until_complete(process_example(example))
loop.close()
