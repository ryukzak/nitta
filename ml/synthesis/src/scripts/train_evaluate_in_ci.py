from pathlib import Path

from components.common.data_loading import load_all_existing_training_data
from components.common.model_loading import load_model
from components.data_crawling.example_running import get_data_for_many_examples_parallel
from components.data_processing.dataset_creation import create_datasets
from components.data_processing.feature_engineering import preprocess_df
from components.model_generation.training import train_and_save_baseline_model
from consts import MODELS_DIR

if __name__ == '__main__':
    model_dir = MODELS_DIR / "production"

    get_data_for_many_examples_parallel([
        Path(r"examples/fibonacci.lua"),
        Path(r"examples/spi2.lua"),
        Path(r"examples/counter.lua"),
        # Path(r"examples/sum.lua"),  # too slow
        # Path(r"examples/constantFolding.lua"),  # too much memory needed
        # Path(r"examples/spi3.lua"),  # too much memory needed
    ])

    try:
        model, meta = load_model(model_dir)
        print("Using manually built model")
        is_manual = True
    except FileNotFoundError:
        print("Training model from scratch")
        is_manual = False

        training_data = load_all_existing_training_data()
        preprocessed = preprocess_df(training_data)
        train_ds, val_ds = create_datasets(preprocessed)
        model, meta = train_and_save_baseline_model(train_ds, val_ds, output_model_name="production")

    # TODO: use TF Asset to save metadata
    with (model_dir / "description.txt").open("w") as f:
        f.write(f"{'Manually' if is_manual else 'Automatically'} trained model for synthesis \n\n")
        f.write(f"Training MAE: {meta.train_mae:.3f}\n")
        f.write(f"Validation MAE: {meta.validation_mae:.3f}\n")
