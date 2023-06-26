import asyncio
import os
import re
import signal
import sys
from asyncio import sleep
from contextlib import asynccontextmanager
from pathlib import Path
from typing import AsyncGenerator, Optional, Tuple

from components.common.logging import get_logger
from components.common.port_management import find_random_free_port
from consts import ROOT_DIR

logger = get_logger(__name__)

_NITTA_START_WAIT_DELAY_S: int = 2
_NITTA_START_RETRY_DELAY_S: int = 2
_NITTA_START_MAX_RETRIES: int = 5
_PORT_IN_NITTA_CMD_REGEX = re.compile(r"(?<=\s)(-p|--port)(=|\s+)\d+")


@asynccontextmanager
async def run_nitta_raw(
    cmd: str,
    env: Optional[dict] = None,
    wait_for_server: bool = False,
    stdout=sys.stdout,
    stderr=sys.stderr,
    max_retries: int = _NITTA_START_MAX_RETRIES,
    change_port_on_retry: bool = False,
) -> AsyncGenerator[asyncio.subprocess.Process, None]:
    env = os.environ.copy()
    env.update(env or {})

    proc = None
    retries_left = max_retries
    try:
        while (proc is None or proc.returncode is not None) and retries_left > 0:
            logger.info(f"Starting NITTA, cmd: {cmd}")

            preexec_fn = (
                None if os.name == "nt" else os.setsid
            )  # see https://stackoverflow.com/a/4791612

            proc = await asyncio.create_subprocess_shell(
                cmd,
                cwd=str(ROOT_DIR),
                stdout=stdout,
                stderr=stderr,
                shell=True,
                preexec_fn=preexec_fn,
                env=env,
            )

            logger.info(f"NITTA has been launched, PID {proc.pid}.")
            if not wait_for_server:
                break

            logger.info(f"Waiting for {_NITTA_START_WAIT_DELAY_S} secs.")
            await sleep(_NITTA_START_WAIT_DELAY_S)

            if proc.returncode is not None:
                logger.warning(
                    f"Failed to start NITTA (exit code {proc.returncode}). "
                    + f"Retrying after a delay of {_NITTA_START_RETRY_DELAY_S} secs (retries left: {retries_left})."
                )
                await sleep(_NITTA_START_RETRY_DELAY_S)

                # Sometimes NITTA fails to start because the given port is already in use (even if it was free
                # just a moment ago), hence we implement a flag that allows us to change the port on retry.
                # It's a bit hacky to re-write the raw given cmd with a regex, but I can't think of a better way that
                # will not hurt the ability to give an arbitrary cmd to run.
                if change_port_on_retry:
                    new_port = find_random_free_port()
                    cmd = _PORT_IN_NITTA_CMD_REGEX.sub(f"-p={new_port}", cmd)

                retries_left -= 1
                proc = None

        if proc is None or proc.returncode is not None:
            raise RuntimeError(
                f"Failed to start NITTA after {_NITTA_START_MAX_RETRIES - retries_left} retries."
            )

        yield proc
    finally:
        if proc is not None and proc.returncode is None:
            pid = proc.pid
            try:
                pgid = os.getpgid(pid)
                logger.info(f"Killing shell and NITTA under it, PID {pid}, PGID {pgid}")
                os.killpg(pgid, signal.SIGTERM)
                await proc.wait()
            except ProcessLookupError:
                # seemingly, NITTA died just after if check
                pass


@asynccontextmanager
async def run_nitta(
    example: Path,
    nitta_run_command: str = "stack exec nitta --",
    nitta_args: str = "",
    given_port: Optional[int] = None,
    **overridden_kwargs,
) -> AsyncGenerator[Tuple[asyncio.subprocess.Process, str], None]:
    port = given_port or find_random_free_port()
    cmd = f"{nitta_run_command} -p={port} {nitta_args} {example}"

    final_kwargs: dict = dict(
        cmd=cmd,
        wait_for_server=True,
        change_port_on_retry=given_port is None,
    )
    final_kwargs.update(overridden_kwargs)

    async with run_nitta_raw(**final_kwargs) as proc:
        nitta_baseurl = f"http://localhost:{port}"
        yield proc, nitta_baseurl
