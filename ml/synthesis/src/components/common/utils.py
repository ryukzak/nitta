import time
from datetime import datetime
from typing import Optional


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
                    print(f"-- skipped {skipped_calls} calls")
                time_last_called = time.time()
                skipped_calls = 0
                return f(*args, **kwargs)
            else:
                skipped_calls += 1

        return wrapped

    return decorate


@debounce(1)
def log_debug(*args):
    # TODO: refactor logging, use stdlib logging module
    print("--", datetime.now().strftime("%T"), *args)


# Source: https://stackoverflow.com/questions/2470971/fast-way-to-test-if-a-port-is-in-use-using-python
def is_port_in_use(port):
    import socket
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        return s.connect_ex(('localhost', port)) == 0


def strip_none_from_tensor_shape(shape):
    return shape[1:] if shape[0] is None else shape
