from pathlib import Path
from time import strftime
from typing import Optional, Tuple

import pandas as pd
from matplotlib import pyplot as plt
from tensorflow.python.data import Dataset
from tensorflow.python.keras.models import Model

from components.common.logging import get_logger
from components.model_generation.model_metainfo import ModelMetainfo
from components.model_generation.models import create_baseline_model
from consts import MODELS_DIR

logger = get_logger(__name__)


def train_and_save_baseline_model(
    train_ds: Dataset,
    val_ds: Dataset,
    fitting_kwargs: Optional[dict] = None,
    output_model_name: Optional[str] = None,
    models_dir: Path = MODELS_DIR,
) -> Tuple[Model, ModelMetainfo]:
    models_dir.mkdir(exist_ok=True)

    sample = next(iter(val_ds))[0][0]
    logger.info(f"Sample input shape: {sample.shape}")

    model = create_baseline_model(input_shape=sample.shape)
    effective_fitting_kwargs = dict(
        epochs=20,
        steps_per_epoch=3000,
    )
    if fitting_kwargs:
        effective_fitting_kwargs.update(fitting_kwargs)
    results = model.fit(x=train_ds, validation_data=val_ds, **effective_fitting_kwargs)

    # TODO: proper model evaluation on an independent dataset
    metainfo = ModelMetainfo(
        train_mae=results.history["mae"][-1],
        validation_mae=results.history["val_mae"][-1],
    )

    if not output_model_name:
        output_model_name = f"model-{strftime('%Y%m%d-%H%M%S')}"

    out_dir = models_dir / output_model_name
    model.save(out_dir)
    with (out_dir / "metainfo.json").open("w") as f:
        f.write(metainfo.json())

    hist_df = pd.DataFrame(results.history)
    hist_df[["loss", "val_loss"]].plot()
    plt.grid()
    hist_df[["mae", "val_mae"]].plot()
    plt.grid()
    plt.savefig(out_dir / "history.png")

    return model, metainfo
