import argparse
import asyncio
import os
import signal
import sys
from asyncio import sleep
from collections import defaultdict
from contextlib import asynccontextmanager
from pathlib import Path
from time import perf_counter
from typing import AsyncGenerator, Tuple

import pandas as pd
from aiohttp import ClientSession, ServerDisconnectedError
from nitta_node import NittaNode


async def retrieve_subforest(
    node: NittaNode,
    session: ClientSession,
    nitta_baseurl: str,
    levels_left=None,
):
    node.children = []
    if node.is_leaf or levels_left == -1:
        return

    async with session.get(f"{nitta_baseurl}/node/{node.sid}/subForest") as resp:
        children_raw = await resp.json()

    for child_raw in children_raw:
        child = NittaNode.from_dict(child_raw)
        child.parent = node
        node.children.append(child)

    levels_left_for_child = None if levels_left is None else levels_left - 1
    await asyncio.gather(
        *[retrieve_subforest(child, session, nitta_baseurl, levels_left_for_child) for child in node.children]
    )


async def retrieve_whole_nitta_tree(nitta_baseurl: str, max_depth=None) -> NittaNode:
    async with ClientSession() as session:
        async with session.get(nitta_baseurl + "/node/-") as resp:
            root_raw = await resp.json()
        root = NittaNode.from_dict(root_raw)
        await retrieve_subforest(root, session, nitta_baseurl, max_depth)

    return root


@asynccontextmanager
async def run_nitta(
    example: Path,
    nitta_exe_path: str = "stack exec nitta -- ",
    nitta_args: str = "",
    nitta_env: dict = None,
    port: int = 8090,
) -> AsyncGenerator[Tuple[asyncio.subprocess.Process, str], None]:

    nitta_baseurl = f"http://localhost:{port}"

    cmd = f"{nitta_exe_path} -p={port} {nitta_args} {example}"

    env = os.environ.copy()
    env.update(nitta_env or {})

    proc = None
    try:
        preexec_fn = None if os.name == "nt" else os.setsid  # see https://stackoverflow.com/a/4791612
        proc = await asyncio.create_subprocess_shell(
            cmd,
            cwd=str("."),
            stdout=sys.stdout,
            stderr=sys.stderr,
            shell=True,
            preexec_fn=preexec_fn,
            env=env,
        )

        await sleep(2)

        yield proc, nitta_baseurl
    finally:
        if proc is not None and proc.returncode is None:
            pid = proc.pid
            pgid = os.getpgid(pid)
            os.killpg(pgid, signal.SIGTERM)
            await proc.wait()


async def select_best_by_evaluator(session, evaluator, node, nitta_baseurl, counters, children_limit=None):
    counters[evaluator.__name__] += 1

    if node.is_leaf:
        if not node.is_finish:
            return None

        return node

    try:
        await retrieve_subforest(node, session, nitta_baseurl)
    except ServerDisconnectedError:
        return None

    children = node.children

    if children_limit:
        children = children[:children_limit]

    children = [(evaluator(child), child) for child in node.children]
    children.sort(key=lambda v: v[0], reverse=True)

    while children:
        next_best_child = children.pop(0)[1]
        result = await select_best_by_evaluator(
            session,
            evaluator,
            next_best_child,
            nitta_baseurl,
            counters,
            children_limit,
        )
        if result is not None:
            return result

    return None


def old_evaluator(node: NittaNode):
    return node.score


def reset_counters():
    global counters
    counters = defaultdict(lambda: 0)


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("example_paths", type=str, nargs="+", help="Paths to the example files")
    parser.add_argument(
        "--nitta_args",
        type=str,
        default="",
        help="Additional arguments for Nitta",
    )
    return parser.parse_args()


async def main(args):
    examples = args.example_paths

    results = []

    for example in examples:
        example = Path(example)

        reset_counters()

        print(f"Selected algorithm: {example}")

        async with run_nitta(example, nitta_args=args.nitta_args) as (
            proc,
            nitta_baseurl,
        ):

            root = await retrieve_whole_nitta_tree(nitta_baseurl)

            async with ClientSession() as session:
                result_dict = {"example": example, "evaluators": {}}
                evaluator = old_evaluator
                start_time = perf_counter()
                best = await select_best_by_evaluator(session, evaluator, root, nitta_baseurl, counters, 2)
                end_time = perf_counter() - start_time
                result_dict["evaluators"]["nitta"] = {
                    "best": best,
                    "duration": best.duration,
                    "depth": best.depth,
                    "evaluator_calls": counters[evaluator.__name__],
                    "time": end_time,
                }
                print("NITTA DONE %s", best)
                print(f"Finished nitta in {end_time:.2f} s")
                results.append(result_dict)

    for result in results:
        print(f"\nAlgorithm: {result['example']}")
        df = pd.DataFrame(
            dict(
                duration=[result["evaluators"]["nitta"]["duration"]],
                depth=[result["evaluators"]["nitta"]["depth"]],
                evaluator_calls=[result["evaluators"]["nitta"]["evaluator_calls"]],
                time=[result["evaluators"]["nitta"]["time"]],
            ),
            index=["nitta"],
        )
        print(df)


if __name__ == "__main__":
    args = parse_args()
    asyncio.run(main(args))
