def constant_to_long(n, unsigned=False):
    assert(isinstance(n, int))
    return _constant_to_size(n, 8, unsigned)


def constant_to_int(n, unsigned=False):
    assert(isinstance(n, int))
    return _constant_to_size(n, 4, unsigned)


def _constant_to_size(n, n_bytes, unsigned):
    # This might not be right for large longs
    b = n.to_bytes(8, byteorder='little', signed=False)
    return int.from_bytes(b[:n_bytes], byteorder='little', signed=not unsigned)
