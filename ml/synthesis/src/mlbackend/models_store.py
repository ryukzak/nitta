from __future__ import annotations

from os import PathLike
from pathlib import Path

from tensorflow.python.keras import Model

from components.common.logging import get_logger
from components.common.model_loading import load_model
from components.model_generation.model_metainfo import ModelMetainfo
from consts import MODELS_DIR

logger = get_logger(__name__)


class ModelsStore:
    """ Loads models on-demand and caches them between calls. """
    model_dir: Path
    models: dict[str, (Model, ModelMetainfo)] = {}

    def __init__(self, model_dir: Path | str | PathLike):
        self.model_dir = model_dir if isinstance(model_dir, Path) else Path(model_dir)

    def __getitem__(self, name: str) -> (Model, ModelMetainfo):
        logger.debug(f"Getting model {name}")
        if name not in self.models:
            logger.debug(f"Cached model {name} not found, loading")

            try:
                self.models[name] = load_model(self.model_dir / name)
            except FileNotFoundError as e:
                raise ModelNotFoundError(f"Model {name} not found in root {MODELS_DIR.absolute()}") from e

        return self.models[name]


class ModelNotFoundError(KeyError):
    def __str__(self):
        return super().__str__().strip("'")


models = ModelsStore(MODELS_DIR)
