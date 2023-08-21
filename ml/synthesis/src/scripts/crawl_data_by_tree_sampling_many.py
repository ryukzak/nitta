import asyncio
import logging

from components.common.logging import configure_logging, get_logger
from components.data_crawling.data_crawling import crawl_data_from_many_examples

logger = get_logger(__name__)

if __name__ == "__main__":
    configure_logging(logging.INFO)
    asyncio.run(crawl_data_from_many_examples())
