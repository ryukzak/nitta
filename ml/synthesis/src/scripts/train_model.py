from components.common.data_loading import load_all_existing_training_data
from components.common.logging import configure_logging, get_logger
from components.data_processing.dataset_creation import create_datasets
from components.data_processing.feature_engineering import preprocess_train_data_df
from components.model_generation.training import train_and_save_baseline_model

# TODO: make it a CLI and merge with train_evaluate_in_ci.py

if __name__ == "__main__":
    logger = get_logger(__name__)
    configure_logging()

    validation_examples = ["pid.lua", "constantFolding.lua"]

    logger.info("Training model from scratch")

    training_data = load_all_existing_training_data()

    test_mask = training_data.example.isin(validation_examples)
    train_df = preprocess_train_data_df(training_data[~test_mask])
    test_df = preprocess_train_data_df(training_data[test_mask])

    # not using test_df from distinct examples (not improving much! not informative?)
    # using random split instead
    test_df = None

    train_ds, val_ds = create_datasets(train_df, test_df)

    model, meta = train_and_save_baseline_model(
        train_ds,
        val_ds,
        fitting_kwargs=dict(validation_steps=3000, epochs=45, steps_per_epoch=3000),
    )
