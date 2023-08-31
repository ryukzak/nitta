from __future__ import annotations

from components.common.logging import configure_logging
from components.model_generation.training import train_and_save_model

if __name__ == "__main__":
    configure_logging()
    train_and_save_model()
