import time
from typing import Optional

from components.common.logging import get_logger

logger = get_logger(__name__)


def debounce(s):
    """
    Decorator ensures function that can only be called once every `s` seconds.
    """

    def decorate(f):
        time_last_called: Optional[float] = None
        skipped_calls = 0

        def wrapped(*args, **kwargs):
            nonlocal time_last_called, skipped_calls
            time_current = time.time()
            if time_last_called is None or time_current - time_last_called >= s:
                if skipped_calls > 0:
                    logger.debug(f"-- skipped {skipped_calls} calls")
                time_last_called = time.time()
                skipped_calls = 0
                return f(*args, **kwargs)
            else:
                skipped_calls += 1

        return wrapped

    return decorate


def strip_none_from_tensor_shape(shape):
    return shape[1:] if shape[0] is None else shape
