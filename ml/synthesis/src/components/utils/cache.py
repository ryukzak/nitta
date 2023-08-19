from cachetools import Cache
from cachetools import cached as lib_cached

_DEFAULT_CACHE_SIZE = 10000


def cached(**overrides):
    def decorator(f):
        return lib_cached(cache=Cache(_DEFAULT_CACHE_SIZE), **overrides)(f)

    return decorator
