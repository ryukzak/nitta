import os
from datetime import datetime
from os import PathLike
from typing import Optional, Union

import pandas as pd

from components.common.logging import get_logger

logger = get_logger(__name__)


def get_current_time_str():
    return datetime.now().strftime("%y%m%d_%H%M%S")


def save_df_with_timestamp(
    df: pd.DataFrame,
    basedir: Union[PathLike, str],
    basename: str,
    what: Optional[str] = None,
    **kwargs,
):
    os.makedirs(basedir, exist_ok=True)
    output = os.path.join(basedir, f"{basename}_{get_current_time_str()}.csv")
    logger.info(f"Saving {what + ' ' if what  else ''}to {output}")
    df.to_csv(output, **kwargs)
