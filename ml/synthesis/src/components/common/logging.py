"""
Do not add non-stdlib imports here and be compatible with Python 3.8.

See ml/synthesis/src/scripts/evaluate_nitta_synthesis.py for more info.
"""
import logging
import os
from logging import Logger


def get_logger(module_name: str) -> Logger:
    """Fixes stdlib's logging function case and create a unified logger factory."""
    if module_name == "__main__":
        return logging.getLogger()  # root logger
    return logging.getLogger(module_name)


def silence_unwanted_logs():
    """Silences DEBUG logs of some libs since they're too detailed."""
    targets = {
        "matplotlib": logging.INFO,
        "tensorflow": logging.ERROR,
    }
    for target, target_level in targets.items():
        target_logger = logging.getLogger(target)
        if target_level > target_logger.getEffectiveLevel():
            logger.info(
                f"Silencing unwanted logs of {target!r} "
                + f"(setting level to {logging.getLevelName(target_level)})...",
            )
            target_logger.setLevel(target_level)
            if target == "tensorflow":
                os.environ["TF_CPP_MIN_LOG_LEVEL"] = "3"


def configure_logging(base_level: int = logging.DEBUG):
    logging.basicConfig(
        format="%(asctime)s %(levelname)s %(name)s %(message)s",
        level=base_level,
    )
    silence_unwanted_logs()


logger = get_logger(__name__)
