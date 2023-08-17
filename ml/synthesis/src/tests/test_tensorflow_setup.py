import os

import tensorflow as tf

from components.common.logging import get_logger


def test_tensorflow_can_do_computations_without_crashing():
    model = tf.keras.applications.VGG19(classes=2, weights=None)
    inputs = [tf.zeros((10, *model.input.shape[1:]))]
    model(inputs)


# pytest may hide important errors printed to stdout (dynamic linking errors, etc.),
# so it may be useful to run this script directly during debugging
if __name__ == "__main__":
    test_tensorflow_can_do_computations_without_crashing()
