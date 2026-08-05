import symbol
import tacky


def address_taken_analysis(symbols, body):
    alised_vars = set()

    def add_if_static(var):
        if isinstance(var, tacky.Identifier):
            sym = symbols[var.name]
            if isinstance(sym.attrs, symbol.StaticAttr):
                alised_vars.add(var.name)

    for instr in body:
        match instr:
            case tacky.GetAddress(src, dst):
                assert(isinstance(src, tacky.Identifier))
                alised_vars.add(src.name)
                add_if_static(dst)
            case tacky.Return(val):
                add_if_static(val)
            case tacky.Truncate(src, dst):
                add_if_static(src)
                add_if_static(dst)
            case tacky.SignExtend(src, dst):
                add_if_static(src)
                add_if_static(dst)
            case tacky.ZeroExtend(src, dst):
                add_if_static(src)
                add_if_static(dst)
            case tacky.DoubleToInt(src, dst):
                add_if_static(src)
                add_if_static(dst)
            case tacky.DoubleToUInt(src, dst):
                add_if_static(src)
                add_if_static(dst)
            case tacky.IntToDouble(src, dst):
                add_if_static(src)
                add_if_static(dst)
            case tacky.UIntToDouble(src, dst):
                add_if_static(src)
                add_if_static(dst)
            case tacky.Unary(_operator, src, dst):
                add_if_static(src)
                add_if_static(dst)
            case tacky.Binary(_operator, left, right, dst):
                add_if_static(left)
                add_if_static(right)
                add_if_static(dst)
            case tacky.Copy(src, dst):
                add_if_static(src)
                add_if_static(dst)
            case tacky.Load(src, dst):
                add_if_static(src)
                add_if_static(dst)
            case tacky.Store(src, dst):
                add_if_static(src)
                add_if_static(dst)
            case tacky.AddPtr(ptr, _index, _scale, dst):
                add_if_static(ptr)
                add_if_static(dst)
            case tacky.CopyToOffset(src, dst, _offset):
                add_if_static(src)
                add_if_static(dst)
            case tacky.CopyFromOffset(src, _offset, dst):
                add_if_static(src)
                add_if_static(dst)
            case tacky.Jump(_target):
                pass
            case tacky.JumpIfZero(condition, _target):
                add_if_static(condition)
            case tacky.JumpIfNotZero(condition, _target):
                add_if_static(condition)
            case tacky.Label(_name):
                pass
            case tacky.Call(_func_name, arg_vals, dst):
                for a in arg_vals:
                    add_if_static(a)
                if dst is not None:
                    add_if_static(dst)
            case _:
                raise Exception(f'unhandled instruction {instr}')

    return alised_vars
