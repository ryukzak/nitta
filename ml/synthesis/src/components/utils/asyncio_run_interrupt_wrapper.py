"""
This module contains a hack/workaround for (apparently) a bug in asyncio.

It hides the unwanted traceback from a SystemExit / KeyboardInterrupt that gets printed if you have:
- an asyncio.run() call in the `main` function of some script,
- an asyncio.gather() that is happening inside the `async_main` function of that asyncio.run(async_main())
- a Ctrl+C, for example, that raises the KeyboardInterrupt inside one of the gathered coroutines.

Even if you handle all the exceptions (including BaseException) inside `async_main`, the KeyboardInterrupt will be
re-raised by asyncio.run() itself, on the level of script's `main`.

Probably related: https://github.com/python/cpython/issues/93122
"""


import asyncio
from functools import wraps


def wrap_reraised_keyboard_interrupt(f):
    @wraps(f)
    def wrapper(*args, **kwargs):
        try:
            return f(*args, **kwargs)
        except (SystemExit, KeyboardInterrupt):
            pass

    return wrapper


def asyncio_run_safe(*args, **kwargs):
    """
    A hack/workaround for (apparently) a bug in asyncio.
    See the module description for details.
    """
    return wrap_reraised_keyboard_interrupt(asyncio.run)(*args, **kwargs)
