from tensorflow.keras import Model, Sequential, layers, optimizers  # pyright: ignore[reportMissingModuleSource]


def create_baseline_model(input_shape) -> Model:
    model = Sequential(
        [
            layers.InputLayer(input_shape=input_shape),
            layers.Dense(128, activation="relu"),
            layers.Dense(64, activation="relu"),
            layers.Dense(64, activation="relu"),
            layers.Dense(32, activation="relu"),
            layers.Dense(1),
        ],
    )

    model.compile(
        optimizer=optimizers.Adam(learning_rate=1e-3),
        loss="mse",
        metrics=["mae"],
    )
    return model
