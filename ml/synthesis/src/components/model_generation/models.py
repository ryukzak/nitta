from tensorflow.keras import Model, Sequential, layers, optimizers  # pyright: ignore[reportMissingModuleSource]


def create_baseline_model(input_shape) -> Model:
    model = Sequential(
        [
            layers.InputLayer(input_shape=input_shape),
            layers.Dense(64, activation="relu", kernel_regularizer="l2"),
            layers.Dropout(0.5),
            layers.Dense(16, activation="relu"),
            layers.Dense(16, activation="relu"),
            layers.Dense(1),
        ],
    )

    model.compile(
        optimizer=optimizers.Adam(learning_rate=1e-4),
        loss="mse",
        metrics=["mae"],
    )
    return model
