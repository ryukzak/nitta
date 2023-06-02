import sys
from pathlib import Path

from components.common.data_loading import load_all_existing_training_data
from components.common.logging import configure_logging, get_logger
from components.common.model_loading import load_model
from components.data_crawling.example_running import get_data_for_many_examples_parallel
from components.data_processing.dataset_creation import create_datasets
from components.data_processing.feature_engineering import preprocess_train_data_df
from components.model_generation.training import train_and_save_baseline_model
from consts import MODELS_DIR

# TODO: make it a CLI and merge with train_evaluate_in_ci.py

if __name__ == "__main__":
    logger = get_logger(__name__)
    configure_logging()

    model_name = "v2_exp1"
    test_examples = ["pid.lua", "constantFolding.lua"]

    model_dir = MODELS_DIR / model_name

    logger.info("Training model from scratch")

    training_data = load_all_existing_training_data()

    test_mask = training_data.example.isin(test_examples)
    train_df = preprocess_train_data_df(training_data[~test_mask])
    test_df = preprocess_train_data_df(training_data[test_mask])

    train_ds, val_ds = create_datasets(train_df, test_df)

    model, meta = train_and_save_baseline_model(
        train_ds,
        val_ds,
        output_model_name=model_name,
        fitting_kwargs=dict(validation_steps=300),
    )

    # TODO: use TF Asset to save metadata
    with (model_dir / "description.txt").open("w") as f:
        f.write(f"Manually trained model for synthesis \n\n")
        f.write(f"Training MAE: {meta.train_mae:.3f}\n")
        f.write(f"Validation MAE: {meta.validation_mae:.3f}\n")
