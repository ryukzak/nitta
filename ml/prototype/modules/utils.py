import time
from datetime import datetime


def debounce(s):
    """Decorator ensures function that can only be called once every `s` seconds.
    """

    def decorate(f):
        t = None
        n = 0

        def wrapped(*args, **kwargs):
            nonlocal t, n
            t_ = time.time()
            if t is None or t_ - t >= s:
                if n > 0: print(f"-- skipped {n} calls")
                result = f(*args, **kwargs)
                t = time.time()
                n = 0
                return result
            else:
                n += 1

        return wrapped

    return decorate


@debounce(1)
def log_debug(*args):
    print("--", datetime.now().strftime("%T"), *args)


# Source: https://stackoverflow.com/questions/2470971/fast-way-to-test-if-a-port-is-in-use-using-python
def is_port_in_use(port):
    import socket
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        return s.connect_ex(('localhost', port)) == 0
