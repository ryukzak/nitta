"""
Do not add non-stdlib imports here and be compatible with Python 3.8.

See ml/synthesis/src/scripts/evaluate_nitta_synthesis.py for more info.
"""
from __future__ import annotations

import csv
from datetime import datetime
from os import PathLike
from pathlib import Path
from typing import TYPE_CHECKING

from components.common.logging import get_logger

if TYPE_CHECKING:
    # hidden to make this module runtime-compatible with stdlib-only evaluate script
    from pandas import DataFrame


logger = get_logger(__name__)


def get_current_time_str():
    return datetime.now().strftime("%y%m%d_%H%M%S")


def _prepare_saving(basedir: PathLike | str, basename: str, what: str | None = None) -> Path:
    basedir = Path(basedir)
    basedir.mkdir(parents=True, exist_ok=True)
    output = basedir / f"{basename}_{get_current_time_str()}.csv"
    logger.info(f"Saving {what + ' ' if what  else ''}to {output}")
    return output


def save_df_with_timestamp(
    df: DataFrame,
    basedir: PathLike | str,
    basename: str,
    what: str | None = None,
    **kwargs,
):
    output = _prepare_saving(basedir, basename, what)
    df.to_csv(output, **kwargs)


def save_dicts_list_to_csv_with_timestamp(
    dicts_list: list[dict],
    basedir: PathLike | str,
    basename: str,
    what: str | None = None,
):
    output = _prepare_saving(basedir, basename, what)
    fields = [] if not dicts_list else dicts_list[0].keys()
    with output.open("w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fields)
        writer.writeheader()
        writer.writerows(dicts_list)
