#!/bin/sh
import argparse
import asyncio
import os
import signal
import sys
from asyncio import sleep
from collections import defaultdict
from contextlib import asynccontextmanager
from dataclasses import dataclass, field
from pathlib import Path
from time import perf_counter
from typing import Any, AsyncGenerator, List, Optional, Tuple

from aiohttp import ClientSession, ServerDisconnectedError  # type: ignore
from dataclasses_json import LetterCase, dataclass_json     # type: ignore

_METRICS_WEIGHTS = {"duration": -1, "depth": -0.1}
_LAMBDA = 0.6

nitta_dataclass_params = dict(letter_case=LetterCase.CAMEL)


@dataclass_json(letter_case=LetterCase.CAMEL)
@dataclass
class NittaNodeDecision:
    tag: str


@dataclass_json(letter_case=LetterCase.CAMEL)
@dataclass
class NittaNode:
    score: Optional[int]
    is_terminal: bool
    is_finish: bool
    parameters: Any
    decision: NittaNodeDecision
    duration: Optional[int]
    sid: str

    children: Optional[List["NittaNode"]] = field(default=None, repr=False)
    parent: Optional["NittaNode"] = field(default=None, repr=False)

    def __hash__(self):
        return hash(self.sid)

    @property
    def is_leaf(self):
        return self.is_terminal

    @property
    def depth(self) -> int:
        return self.sid.count("-") if self.sid != "-" else 0


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
        child = NittaNode.from_dict(child_raw)  # type: ignore
        child.parent = node
        node.children.append(child)

    levels_left_for_child = None if levels_left is None else levels_left - 1
    await asyncio.gather(
        *[retrieve_subforest(child, session, nitta_baseurl, levels_left_for_child) for child in node.children]
    )


async def retrieve_tree_root(nitta_baseurl: str) -> NittaNode:
    async with ClientSession() as session:
        async with session.get(nitta_baseurl + "/node/-") as resp:
            root_raw = await resp.json()
            print()
        root = NittaNode.from_dict(root_raw)  # type: ignore
    return root


@asynccontextmanager
async def run_nitta(
    example: Path,
    nitta_exe_path: str = "stack exec nitta -- ",
    nitta_args: str = "",
    nitta_env: dict = {},
    port: int = 8092,
) -> AsyncGenerator[Tuple[asyncio.subprocess.Process, str], None]:
    nitta_baseurl = f"http://localhost:{port}"

    cmd = f"{nitta_exe_path} -p={port} {nitta_args} {example}"
    print(cmd)
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

    result_list = []  # List to hold all results

    for example in examples:
        example_path = Path(example)

        reset_counters()

        print(f"Selected algorithm: {example_path.stem}")

        async with run_nitta(example_path, nitta_args=args.nitta_args) as (
            proc,
            nitta_baseurl,
        ):
            root = await retrieve_tree_root(nitta_baseurl)
            async with ClientSession() as session:
                evaluator = old_evaluator
                start_time = perf_counter()
                best = await select_best_by_evaluator(session, evaluator, root, nitta_baseurl, counters, 2)
                end_time = perf_counter() - start_time
                result_dict = {
                    "example_name": example_path.stem,
                    "duration": best.duration,
                    "depth": best.depth,
                    "evaluator_calls": counters[evaluator.__name__],
                    "time": end_time,
                }
                print(f"{example_path.stem.upper()} DONE {best}")
                print(f"Finished {example_path.stem} in {end_time:.2f} s")

                result_list.append(result_dict)

    # Print header
    print("{:<12} {:<10} {:<16} {:<16} {:<10}".format("Name", "Duration", "Depth", "Evaluator Calls", "Time"))

    # Print each data item.
    for data_dict in result_list:
        print(
            "{:<12} {:<10} {:<16} {:<16} {:<10}".format(
                data_dict["example_name"],
                data_dict["duration"],
                data_dict["depth"],
                data_dict["evaluator_calls"],
                data_dict["time"],
            )
        )


if __name__ == "__main__":
    args = parse_args()
    asyncio.run(main(args))
