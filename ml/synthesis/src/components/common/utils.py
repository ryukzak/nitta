from __future__ import annotations

from components.common.logging import get_logger

logger = get_logger(__name__)

# TODO: move to tf-related utils module


def strip_none_from_tensor_shape(shape):
    return shape[1:] if shape[0] is None else shape
