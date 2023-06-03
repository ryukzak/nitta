import json
from pathlib import Path
from typing import Tuple

import tensorflow as tf
from components.common.logging import get_logger
from components.model_generation.model_metainfo import ModelMetainfo

logger = get_logger(__name__)


def load_model(model_dir: Path) -> Tuple[tf.keras.models.Model, ModelMetainfo]:
    logger.debug(f"Loading model from {model_dir}...")
    with (model_dir / "metainfo.json").open("r") as f:
        return tf.keras.models.load_model(model_dir), ModelMetainfo.from_dict(
            json.load(f)
        )
