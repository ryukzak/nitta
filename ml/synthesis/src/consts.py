"""
Do not add non-stdlib imports here and be compatible with Python 3.8.

See ml/synthesis/src/scripts/evaluate_nitta_synthesis.py for more info.
"""
import os
from pathlib import Path


class EnvVarNames:
    MODELS_DIR = "NITTA_ML_SYNTHESIS_MODELS_DIR"


ROOT_DIR = Path()
EXAMPLES_DIR = Path("examples")

ML_SYNTHESIS_DIR = Path("ml/synthesis")
DATA_DIR = ML_SYNTHESIS_DIR / "data"

_models_dir_env = os.environ.get(EnvVarNames.MODELS_DIR)
MODELS_DIR = Path(_models_dir_env) if _models_dir_env else ML_SYNTHESIS_DIR / "models"

ML_BACKEND_BASE_URL_FILEPATH = ".ml_backend_base_url"
