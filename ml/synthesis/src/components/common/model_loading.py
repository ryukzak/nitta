from __future__ import annotations

import json
from pathlib import Path

from tensorflow.python.keras import Model
from tensorflow.python.keras.models import load_model  # pyright: ignore[reportMissingImports]

from components.common.logging import get_logger
from components.model_generation.model_metainfo import ModelMetainfo

logger = get_logger(__name__)


def load_model_with_metainfo(model_dir: Path, not_for_training: bool = True) -> tuple[Model, ModelMetainfo]:
    logger.debug(f"Loading model from {model_dir}...")

    model = load_model(
        model_dir,
        compile=False if not_for_training else True,  # readability!
    )

    with (model_dir / "metainfo.json").open("r") as f:
        metainfo = ModelMetainfo.parse_obj(json.load(f))

    return model, metainfo
