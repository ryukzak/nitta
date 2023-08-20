from __future__ import annotations

from components.common.data_loading import load_all_existing_training_data
from components.common.logging import configure_logging, get_logger
from components.data_processing.dataset_creation import create_datasets
from components.data_processing.feature_engineering import preprocess_input_data_df
from components.model_generation.training import train_and_save_baseline_model

_DEFAULT_VALIDATION_EXAMPLES = ["pid.lua", "constantFolding.lua"]

# TODO: make it a CLI and merge with train_evaluate_in_ci.py

if __name__ == "__main__":
    logger = get_logger(__name__)
    configure_logging()

    logger.info("Training model from scratch")

    training_data = load_all_existing_training_data()

    val_mask = training_data.example.isin(_DEFAULT_VALIDATION_EXAMPLES)
    train_df = preprocess_input_data_df(training_data[~val_mask])
    val_df = preprocess_input_data_df(training_data[val_mask])

    train_ds, val_ds, input_cols = create_datasets(train_df, val_df)

    model, meta = train_and_save_baseline_model(
        train_ds,
        val_ds,
        input_cols,
        fitting_kwargs=dict(validation_steps=3000, epochs=45, steps_per_epoch=3000),
    )
