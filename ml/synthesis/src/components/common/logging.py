"""
Do not add non-stdlib imports here and be compatible with Python 3.8.

See ml/synthesis/src/scripts/evaluate_nitta_synthesis.py for more info.
"""
import logging
from logging import Logger


def get_logger(module_name: str) -> Logger:
    """Fixes stdlib's logging function case and create a unified logger factory."""
    if module_name == "__main__":
        return logging.getLogger()  # root logger
    return logging.getLogger(module_name)


def silence_unwanted_logs():
    """Silences DEBUG logs of some libs since they're too detailed."""
    targets = ["matplotlib"]
    for target in targets:
        logger.debug(f"Silencing DEBUG logs of {target!r}...")
        logging.getLogger(target).setLevel(logging.INFO)


def configure_logging(base_level: int = logging.DEBUG):
    logging.basicConfig(
        format="%(asctime)s %(levelname)s %(name)s %(message)s",
        level=base_level,
    )
    if base_level == logging.DEBUG:
        silence_unwanted_logs()


logger = get_logger(__name__)
