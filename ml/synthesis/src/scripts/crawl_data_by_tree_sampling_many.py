import asyncio
import logging

from components.common.logging import configure_logging, get_logger
from components.data_crawling.example_running import produce_data_for_many_examples

logger = get_logger(__name__)

if __name__ == "__main__":
    configure_logging(base_level=logging.INFO)
    asyncio.run(produce_data_for_many_examples())
