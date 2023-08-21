from __future__ import annotations

from pathlib import Path

import pandas as pd
from matplotlib import pyplot as plt
from tensorflow.data import Dataset  # pyright: ignore[reportMissingModuleSource]
from tensorflow.python.keras import Model

from components.common.logging import get_logger
from components.common.saving import get_current_time_str
from components.model_generation.model_metainfo import ModelMetainfo
from components.model_generation.models import create_baseline_model
from consts import MODELS_DIR

logger = get_logger(__name__)


def train_and_save_baseline_model(
    train_ds: Dataset,
    val_ds: Dataset,
    input_cols: list[str],
    fitting_kwargs: dict | None = None,
    output_model_name: str | None = None,
    models_dir: Path = MODELS_DIR,
) -> tuple[Model, ModelMetainfo]:
    models_dir.mkdir(exist_ok=True)

    sample = next(iter(val_ds))[0][0]
    logger.info(f"Sample input shape: {sample.shape}")

    model = create_baseline_model(input_shape=sample.shape)
    effective_fitting_kwargs = dict(
        epochs=45,
        steps_per_epoch=3000,
    )
    if fitting_kwargs:
        effective_fitting_kwargs.update(fitting_kwargs)

    try:
        model.fit(x=train_ds, validation_data=val_ds, **effective_fitting_kwargs)
    except KeyboardInterrupt:
        logger.info("\n\n=== Training interrupted by user, saving the model in a current state ===")
    history = model.history.history

    if not output_model_name:
        output_model_name = f"model_{get_current_time_str()}"
    out_dir = models_dir / output_model_name
    out_metainfo = out_dir / "metainfo.json"
    out_history_csv = out_dir / "history.csv"
    out_history_png = out_dir / "history.png"

    logger.info(f"Saving the MODEL: {out_dir}")
    model.save(out_dir)

    logger.info("Evaluating the model on the validation dataset...")
    evaluation_results: dict = model.evaluate(val_ds, return_dict=True)
    logger.info(f"Evaluation results: {evaluation_results}")

    logger.info(f"Saving the MODEL METAINFO: {out_metainfo}")
    metainfo = ModelMetainfo(
        train_mae=history["mae"][-1],
        validation_mae=evaluation_results["mae"],
        input_columns=input_cols,
    )
    with out_metainfo.open("w") as f:
        f.write(metainfo.json())

    logger.info(f"Saving the MODEL TRAINING HISTORY DATA: {out_history_csv}")
    hist_df = pd.DataFrame(history)
    hist_df.to_csv(out_history_csv, index=False)

    logger.info(f"Saving the MODEL TRAINING HISTORY CHART: {out_history_png}")
    fig, ax = plt.subplots(1, 2, figsize=(12, 6))
    hist_df[["loss", "val_loss"]].plot(ax=ax[0])
    ax[0].grid()
    hist_df[["mae", "val_mae"]].plot(ax=ax[1])
    ax[1].grid()
    fig.savefig(out_history_png)

    logger.info(f"Model training done: {out_dir}")
    return model, metainfo
