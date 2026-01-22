import syntax


def type_size(t):
    if t == syntax.Long() or t == syntax.ULong() or t == syntax.Double():
        return 8
    if t == syntax.Int() or t == syntax.UInt():
        return 4
    raise Exception(f'Unhandled type to get size of {repr(t)}')


def is_signed(t):
    if t == syntax.Long() or t == syntax.Int():
        return True
    if t == syntax.ULong() or t == syntax.UInt():
        return False
    raise Exception(f'Unhandled type to get signedness of {t}')


def constant_to_long(n, unsigned=False):
    n = int(n)
    return _constant_to_size(n, 8, unsigned)


def constant_to_int(n, unsigned=False):
    n = int(n)
    return _constant_to_size(n, 4, unsigned)


def _constant_to_size(n, n_bytes, unsigned):
    # This might not be right for large longs
    b = n.to_bytes(8, byteorder='little', signed=False)
    return int.from_bytes(b[:n_bytes], byteorder='little', signed=not unsigned)
