def strip_none_from_tensor_shape(shape):
    return shape[1:] if shape[0] is None else shape
