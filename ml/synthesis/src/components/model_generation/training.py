from __future__ import annotations

from pathlib import Path

import pandas as pd
from matplotlib import pyplot as plt
from tensorflow.data import Dataset  # pyright: ignore[reportMissingModuleSource]
from tensorflow.python.keras import Model

from components.common.data_loading import load_all_existing_training_data
from components.common.logging import get_logger
from components.common.saving import get_current_time_str
from components.data_crawling.data_crawling import MANUAL_FULL_CRAWL_CONFIG_VALIDATION_EXAMPLES
from components.data_processing.dataset_creation import create_datasets
from components.data_processing.feature_engineering import preprocess_input_data_df
from components.model_generation.model_metainfo import ModelMetainfo
from components.model_generation.models import create_baseline_model
from consts import MODELS_DIR

logger = get_logger(__name__)


def train_and_save_model(
    model_name: str | None = None,
    val_examples: list[str] = MANUAL_FULL_CRAWL_CONFIG_VALIDATION_EXAMPLES,
):
    logger.info("Training a new model using all available training data")

    training_data = load_all_existing_training_data()

    val_mask = training_data.example.isin(val_examples)
    train_df = preprocess_input_data_df(training_data[~val_mask])
    val_df = preprocess_input_data_df(training_data[val_mask])

    train_ds, val_ds, input_cols = create_datasets(train_df, val_df)

    return train_and_save_model_on_given_data(train_ds, val_ds, input_cols, output_model_name=model_name)


def train_and_save_model_on_given_data(
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
        epochs=30,
        steps_per_epoch=1500,
        validation_steps=3000,
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
