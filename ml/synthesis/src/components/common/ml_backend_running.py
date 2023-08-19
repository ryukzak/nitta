import asyncio
import sys
from asyncio.subprocess import Process
from contextlib import asynccontextmanager
from typing import AsyncGenerator, Tuple

from aiohttp import ClientSession

from components.common.logging import get_logger
from consts import ML_BACKEND_BASE_URL_FILEPATH, ROOT_DIR

logger = get_logger(__name__)


@asynccontextmanager
async def run_ml_backend() -> AsyncGenerator[Tuple[Process, str], None]:
    proc = None
    try:
        executable = sys.executable  # current Python interpreter
        executable_args = ["-m", "mlbackend"]
        logger.info(f"Starting ML backend server, command: {executable} {' '.join(executable_args)}")

        proc = await asyncio.create_subprocess_exec(
            executable,
            *executable_args,
            cwd=str(ROOT_DIR),
            stdout=sys.stdout,
            stderr=sys.stderr,
        )

        logger.info(f"ML backend server has been launched, PID {proc.pid}.")

        logger.info("Waiting for ML backend server to start...")
        while not ROOT_DIR.joinpath(ML_BACKEND_BASE_URL_FILEPATH).exists():
            await asyncio.sleep(0.1)
        with ROOT_DIR.joinpath(ML_BACKEND_BASE_URL_FILEPATH).open("r") as f:
            base_url = f.read().strip()
        logger.info(f"Found a base URL: {base_url}. Trying to reach the API...")

        could_reach_api = False
        async with ClientSession() as session:
            while not could_reach_api:
                try:
                    async with session.get(base_url + "/docs") as resp:
                        resp.raise_for_status()
                    could_reach_api = True
                except Exception:
                    await asyncio.sleep(0.3)

        logger.info("ML backend server is ready to serve requests.")

        yield proc, base_url
    finally:
        if proc is not None and proc.returncode is None:
            logger.info(f"Killing ML backend server, PID {proc.pid}")
            proc.terminate()
            await proc.communicate()
