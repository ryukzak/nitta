"""
Do not add non-stdlib imports here and be compatible with Python 3.8.

See ml/synthesis/src/scripts/evaluate_nitta_synthesis.py for more info.
"""
import asyncio
import os
import re
import signal
from asyncio import Lock
from asyncio.subprocess import PIPE, STDOUT, Process
from contextlib import asynccontextmanager
from dataclasses import dataclass
from pathlib import Path
from typing import AsyncGenerator, Optional

from components.common.logging import get_logger
from consts import ROOT_DIR

logger = get_logger(__name__)
nitta_passthrough_logger = get_logger("nitta")

_NITTA_SERVER_START_REGEX = re.compile(
    r"Running NITTA server at http:\/\/localhost:(?P<port>\d+)"
)


@dataclass
class NittaRunResult:
    proc: Process

    _port_reading_lock = Lock()
    _port: Optional[int] = None

    async def get_port(self) -> int:
        if self._port is None:
            logger.debug(f"Parsing NITTA API port for PID {self.proc.pid}")
            async with self._port_reading_lock:
                while self._port is None:
                    assert (
                        self.proc.stdout is not None
                    ), "stdout required to read NITTA port"
                    line_bytes = await self.proc.stdout.readline()
                    if not line_bytes:
                        raise RuntimeError(
                            f"Couldn't find NITTA API server port, EOF reached"
                        )
                    line = line_bytes.decode("utf-8").strip()
                    nitta_passthrough_logger.info(line)

                    match = _NITTA_SERVER_START_REGEX.search(line)
                    if match:
                        self._port = int(match.group("port"))
                        logger.debug(
                            f"Got a NITTA port regex match, port: {self._port}"
                        )
                        break

        return self._port

    async def get_base_url(self) -> str:
        return f"http://localhost:{await self.get_port()}"


@asynccontextmanager
async def run_nitta(
    full_shell_cmd: str,
    env: Optional[dict] = None,
    stdout=PIPE,
    stderr=STDOUT,
) -> AsyncGenerator[NittaRunResult, None]:
    env = os.environ.copy()
    env.update(env or {})

    proc = None
    try:
        logger.info(f"Starting NITTA, command: {full_shell_cmd}")

        preexec_fn = (
            None if os.name == "nt" else os.setsid
        )  # see https://stackoverflow.com/a/4791612

        proc = await asyncio.create_subprocess_shell(
            full_shell_cmd,
            cwd=str(ROOT_DIR),
            stdout=stdout,
            stderr=stderr,
            shell=True,
            preexec_fn=preexec_fn,
            env=env,
        )

        logger.info(f"NITTA has been launched, PID {proc.pid}.")

        yield NittaRunResult(proc)
    finally:
        if proc is not None and proc.returncode is None:
            pid = proc.pid
            try:
                pgid = os.getpgid(pid)
                logger.info(f"Killing shell and NITTA under it, PID {pid}, PGID {pgid}")
                os.killpg(pgid, signal.SIGTERM)
                await proc.communicate()
                # like wait(), but reading stdout/stderr until EOF first to avoid PIPE-related deadlocks
            except ProcessLookupError:
                # NITTA died just after the check in the if statement above?
                # other possible reasons why returncode is None?
                pass


@asynccontextmanager
async def run_nitta_server(
    example: Path,
    nitta_run_command: str = "stack exec nitta --",
    nitta_args: str = "--method=NoSynthesis",
    **overridden_kwargs,
) -> AsyncGenerator[NittaRunResult, None]:
    port = 0  # NITTA will choose a random free port and print it to stdout, we'll parse

    final_kwargs: dict = dict(
        full_shell_cmd=f"{nitta_run_command} -p={port} {nitta_args} {example}",
    )
    final_kwargs.update(overridden_kwargs)

    async with run_nitta(**final_kwargs) as result:
        yield result
