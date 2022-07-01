import json
from pathlib import Path
from typing import Tuple

import tensorflow as tf

from components.model_generation.model_metainfo import ModelMetainfo


def load_model(model_dir: Path) -> Tuple[tf.keras.models.Model, ModelMetainfo]:
    with (model_dir / "metainfo.json").open("r") as f:
        return tf.keras.models.load_model(model_dir), ModelMetainfo.from_dict(json.load(f))
