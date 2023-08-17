"""
Do not add non-stdlib imports here and be compatible with Python 3.8.

See ml/synthesis/src/scripts/evaluate_nitta_synthesis.py for more info.
"""
import asyncio
import os
import re
import signal
from asyncio import Event, Task
from asyncio.subprocess import PIPE, STDOUT, Process
from contextlib import asynccontextmanager
from logging import Logger
from pathlib import Path
from typing import AsyncGenerator, Optional

from components.common.logging import get_logger
from consts import ROOT_DIR

logger = get_logger(__name__)

_NITTA_SERVER_START_REGEX = re.compile(
    r"Running NITTA server at http:\/\/localhost:(?P<port>\d+)"
)


class NittaRunResult:
    proc: Process
    stdout_pipe_reader: Task

    _port_found_event: Event
    _port: int
    _passthrough_logger: Logger

    def __init__(self, proc: Process):
        self.proc = proc
        self.stdout_pipe_reader = asyncio.create_task(self._stdout_pipe_reader_job())

        self._port_found_event = Event()
        self._passthrough_logger = get_logger(f"nitta.{proc.pid}")

    async def _stdout_pipe_reader_job(self):
        try:
            if self.proc.stdout is None:
                logger.warning(
                    f"NITTA-{self.proc.pid} stdout is None (=no logs or server port will be ever read), "
                    + "keep that in mind"
                )
                return

            while self.proc.returncode is None:
                line_bytes = await self.proc.stdout.readline()
                if not line_bytes:
                    logger.info(f"NITTA-{self.proc.pid} stdout pipe EOF reached")
                    break
                line = line_bytes.decode("utf-8").strip()
                self._passthrough_logger.info(line)
                self._check_new_stdout_line_for_server_port(line)
        finally:
            logger.info(f"NITTA-{self.proc.pid} stdout pipe reader is done")

    def _check_new_stdout_line_for_server_port(self, line: str):
        match = _NITTA_SERVER_START_REGEX.search(line)
        if match:
            self._port = int(match.group("port"))
            logger.debug(f"Got a NITTA port regex match, port: {self._port}")
            self._port_found_event.set()

    async def get_port(self) -> int:
        if not self._port_found_event.is_set():
            if self.stdout_pipe_reader.done():
                raise RuntimeError(
                    f"Couldn't read NITTA API server port, it wasn't read earlier and stdout pipe reader is done"
                )

            logger.debug(f"Waiting for NITTA API port to be read (PID {self.proc.pid})")
            await self._port_found_event.wait()

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
    run_result = None
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

        run_result = NittaRunResult(proc)
        yield run_result
    finally:
        if proc is not None and proc.returncode is None:
            pid = proc.pid
            try:
                pgid = os.getpgid(pid)
                logger.info(f"Killing shell and NITTA under it, PID {pid}, PGID {pgid}")
                os.killpg(pgid, signal.SIGTERM)

                if run_result is not None:
                    logger.info(f"Waiting for NITTA-{pid} stdout pipe reader to finish")
                    await run_result.stdout_pipe_reader

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
