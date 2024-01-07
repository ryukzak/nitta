"""
Do not add non-stdlib imports here and be compatible with Python 3.8.

See ml/synthesis/src/scripts/evaluate_nitta_synthesis.py for more info.
"""
import os
from pathlib import Path


class EnvVarNames:
    MODELS_DIR = "NITTA_ML_SYNTHESIS_MODELS_DIR"
    NITTA_RUN_COMMAND_OVERRIDE = "NITTA_RUN_COMMAND_OVERRIDE"

_examples_dir_name = "examples"


def _find_root_dir():
    """
    Implements dynamic repo root finding if the current working dir is suddenly not it.

    This makes life easier since it's impossible to configure some tools the other way or without other nasty hacks.

    For example, a VS code test explorer extension at the moment of writing does not allow to specify tests cwd and
    pytest's --rootdir (used for grabbing config) independently, hence the runtime tests cwd is <root>/ml/synthesis.

    Happens because of this:
    https://github.com/kondratyev-nv/vscode-python-test-adapter/issues/198#issuecomment-708671658

    Other option would be to move pyproject.toml to repo root, but it seems completely wrong. File an issue?
    """

    candidate = Path().resolve()
    while not (candidate / _examples_dir_name).exists() and candidate.parent != candidate:
        candidate = candidate.parent

    return candidate if (candidate / _examples_dir_name).exists() else Path()


ROOT_DIR = _find_root_dir()

EXAMPLES_DIR = ROOT_DIR / _examples_dir_name
EVALUATIONS_DIR = ROOT_DIR / "evaluation"

ML_SYNTHESIS_DIR = ROOT_DIR / "ml" / "synthesis"
DATA_DIR = ML_SYNTHESIS_DIR / "data"
EVALUATION_CONFIGS_DIR = ML_SYNTHESIS_DIR / "src" / "scripts" / "evaluation_configs"

_models_dir_env = os.environ.get(EnvVarNames.MODELS_DIR)
MODELS_DIR = Path(_models_dir_env) if _models_dir_env else ML_SYNTHESIS_DIR / "models"

ML_BACKEND_BASE_URL_FILEPATH = ".ml_backend_base_url"

# high priority env var which overrides command provided in the config file
NITTA_RUN_COMMAND_OVERRIDE = os.environ.get(EnvVarNames.NITTA_RUN_COMMAND_OVERRIDE, None)
