import logging
from logging import Logger


def get_logger(module_name: str) -> Logger:
    """Fixes stdlib's logging function case and create a unified logger factory."""
    if module_name == "__main__":
        return logging.getLogger()  # root logger
    return logging.getLogger(module_name)


def configure_logging(base_level: int = logging.DEBUG):
    logging.basicConfig(
        format="%(asctime)s %(levelname)s %(name)s %(message)s",
        level=base_level,
    )


def set_logging_level(module_name: str, level: int):
    logger = get_logger(module_name)
    logger.setLevel(level)
