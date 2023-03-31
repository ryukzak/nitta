import os
from pathlib import Path


class EnvVarNames:
    MODELS_DIR = "NITTA_ML_SYNTHESIS_MODELS_DIR"


ROOT_DIR = Path(".")
EXAMPLES_DIR = Path("examples")

ML_SYNTHESIS_DIR = Path("ml/synthesis")
DATA_DIR = ML_SYNTHESIS_DIR / "data"

MODELS_DIR = os.environ.get(EnvVarNames.MODELS_DIR)
MODELS_DIR = Path(MODELS_DIR) if MODELS_DIR else ML_SYNTHESIS_DIR / "models"

ML_BACKEND_BASE_URL_FILEPATH = ".ml_backend_base_url"
