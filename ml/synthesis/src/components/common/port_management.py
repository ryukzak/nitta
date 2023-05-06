import random
import socket

from components.common.logging import get_logger

logger = get_logger(__name__)

_RANDOM_PORT_RANGE = (32768, 60999)


# Source: https://stackoverflow.com/questions/2470971/fast-way-to-test-if-a-port-is-in-use-using-python
def is_port_in_use(port):
    logger.debug(f"Finding out if port {port} is in use...")
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        result = s.connect_ex(("localhost", port)) == 0
        logger.debug(f"Port {port} {'is in use' if result else 'is free'}")
        return result


def find_random_free_port() -> int:
    logger.debug("Finding random free port...")
    final_port = None
    while not final_port:
        port_candidate = random.randint(*_RANDOM_PORT_RANGE)
        if not is_port_in_use(port_candidate):
            final_port = port_candidate
    logger.info(f"Found random free port: {final_port}")
    return final_port
