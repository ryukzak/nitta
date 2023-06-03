from __future__ import annotations

import os
from contextlib import AbstractContextManager
from os import PathLike
from pathlib import Path
from types import TracebackType
from typing import Type

from components.common.logging import get_logger
from components.common.port_management import find_random_free_port

logger = get_logger(__name__)


class BackendBaseUrlFile(AbstractContextManager):
    """
    A context manager to share a public base url for active backend server instance.

    Manages a file that can be read by server clients (namely, Haskell part of NITTA when it looks for ML backend server):
        - to determine if a server instance has started successfully and is currently running;
        - to find the server's base URL.

    Base URL format must contain {port} placeholder, which will be replaced with the actual chosen random unbound port.

    Usage example:

        with BackendBaseUrlFile(".ml_backend_base_url", "http://localhost:{port}/mlbackend") as base_url_file:
            server.start(port=base_url_file.port)
    """

    filepath: Path
    port: int | None
    _base_url_fmt: str

    def __init__(self, filepath: str | PathLike, base_url_fmt: str):
        self.filepath = Path(filepath)
        self.port = None
        self._base_url_fmt = base_url_fmt

    def __enter__(self) -> "BackendBaseUrlFile":
        if self.filepath.exists():
            with self.filepath.open("r") as f:
                old_base_url = f.read()
            logger.warning(
                f"Backend server is already running or exited unexpectedly. "
                f"Trying to recover correct state. "
                f"Will overwrite the old base URL ({old_base_url}) in file ({self.filepath.absolute()}). "
            )

        self.port = find_random_free_port()

        base_url = self._base_url_fmt.format(port=self.port)
        logger.info(f"Writing base URL ({base_url}) to file ({self.filepath.absolute()}).")
        with self.filepath.open("w") as f:
            f.write(base_url + "\n")
        return self

    def __exit__(
        self,
        __exc_type: Type[BaseException] | None,
        __exc_value: BaseException | None,
        __traceback: TracebackType | None,
    ) -> bool | None:
        if not self.filepath.exists():
            logger.warning(
                f"Exiting, so wanted to remove {self.filepath.absolute()}, " f"but it does not exist. Doing nothing."
            )
            return

        logger.info(f"Exiting, so removing {self.filepath.absolute()}.")
        os.remove(self.filepath)

        return False  # do not suppress exceptions
