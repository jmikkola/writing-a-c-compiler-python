import syntax


def type_size(t, types):
    match t:
        case syntax.Char() | syntax.UChar() | syntax.SChar():
            return 1
        case syntax.Int() | syntax.UInt():
            return 4
        case syntax.Long() | syntax.ULong():
            return 8
        case syntax.Double():
            return 8
        case syntax.Pointer():
            return 8
        case syntax.Array(elem_t, size):
            return size * type_size(elem_t, types)
        case syntax.Struct(tag):
            return types[tag].size
        case syntax.Union(tag):
            return types[tag].size
        case syntax.Func():
            raise Exception('cannot determine size of a function type')
        case _:
            raise Exception(f'Unhandled type to get size of {repr(t)}')


def is_signed(t):
    if t == syntax.Long() or t == syntax.Int() or t == syntax.Char() or t == syntax.SChar():
        return True
    if t == syntax.ULong() or t == syntax.UInt() or t == syntax.UChar():
        return False
    if t == syntax.Double():
        return True
    if isinstance(t, syntax.Pointer):
        return False
    raise Exception(f'Unhandled type to get signedness of {t}')


def alignment_of(var_type: syntax.Type, types) -> int:
    match var_type:
        case syntax.Char() | syntax.UChar() | syntax.SChar():
            return 1
        case syntax.Int() | syntax.UInt():
            return 4
        case syntax.Long() | syntax.ULong() | syntax.Double() | syntax.Pointer():
            return 8
        case syntax.Array(elem_t, _):
            size = type_size(var_type, types)
            if size >= 16:
                return 16
            else:
                scalar = find_scalar(elem_t)
                return alignment_of(scalar, types)
        case syntax.Struct(tag):
            return types[tag].alignment
        case syntax.Union(tag):
            return types[tag].alignment
        case _:
            raise Exception(f'Unexpected type to find alignment of {var_type}')


def find_scalar(var_type: syntax.Type) -> syntax.Type:
    ''' e.g. find_scalar(int[2][3][4]) == int '''
    match var_type:
        case syntax.Array(elem_t, _):
            return find_scalar(elem_t)
        case _:
            return var_type


def constant_to_long(n, unsigned=False):
    n = int(n)
    return _constant_to_size(n, 8, unsigned)


def constant_to_int(n, unsigned=False):
    n = int(n)
    return _constant_to_size(n, 4, unsigned)


def constant_to_byte(n, unsigned=False):
    n = int(n)
    return _constant_to_size(n, 1, unsigned)


def _constant_to_size(n, n_bytes, unsigned):
    # This might not be right for large longs
    b = n.to_bytes(8, byteorder='little', signed=not unsigned)
    return int.from_bytes(b[:n_bytes], byteorder='little', signed=not unsigned)
