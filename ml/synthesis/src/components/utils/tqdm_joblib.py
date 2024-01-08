"""
Copied from https://github.com/louisabraham/tqdm_joblib/tree/d690624655f8e8b7628a39a30f330a7630915c73
and modified due to the lack of required pbar_update_multiplier functionality in the original.
"""
import contextlib

import joblib
from tqdm.autonotebook import tqdm


@contextlib.contextmanager
def tqdm_joblib(pbar_update_multiplier: int = 1, *args, **kwargs):
    """Context manager to patch joblib to report into tqdm progress bar
    given as argument"""

    tqdm_object = tqdm(*args, **kwargs)

    class TqdmBatchCompletionCallback(joblib.parallel.BatchCompletionCallBack):
        def __init__(self, *args, **kwargs):
            super().__init__(*args, **kwargs)

        def __call__(self, *args, **kwargs):
            tqdm_object.update(n=self.batch_size * pbar_update_multiplier)
            return super().__call__(*args, **kwargs)

    old_batch_callback = joblib.parallel.BatchCompletionCallBack
    joblib.parallel.BatchCompletionCallBack = TqdmBatchCompletionCallback
    try:
        yield tqdm_object
    finally:
        joblib.parallel.BatchCompletionCallBack = old_batch_callback
        tqdm_object.close()
