import tacky


def optimize(ir: tacky.Program, args):
    optimized = []
    for decl in ir.top_level:
        match decl:
            case tacky.Function():
                optimized.append(optimize_function(decl, args))
            case _:
                optimized.append(decl)
    return tacky.Program(optimized)


def optimize_function(f: tacky.Function, args):
    body = f.body
    if not body:
        return f

    while True:
        if args.fold_constants:
            post_constant_folding = constant_folding(body)
        else:
            post_constant_folding = body

        cfg = make_control_flow_graph(post_constant_folding)

        if args.eliminate_unreachable_code:
            cfg = unreachable_code_elimination(cfg)

        if args.propagate_copies:
            cfg = copy_propagation(cfg)

        if args.eliminate_dead_stores:
            cfg = dead_store_elimination(cfg)

        optimized_function_body = cfg_to_instructions(cfg)

        if optimized_function_body == body:
            break
        if not optimized_function_body:
            break

        body = optimized_function_body

    return tacky.Function(f.name, f.is_global, f.params, body)


def constant_folding(body):
    optimized = []
    for instr in body:
        match instr:
            case tacky.Unary(op, tacky.Constant(const), dst):
                new_const = evaluate_unary_op(op, const)
                optimized.append(tacky.Copy(new_const, dst))
            case tacky.Binary(op, tacky.Constant(left), tacky.Constant(right), dst):
                new_const = evaluate_binary_op(op, left, right)
                optimized.append(tacky.Copy(new_const, dst))
            case tacky.JumpIfZero(tacky.Constant(const), target):
                if is_zero(const):
                    optimized.append(tacky.Jump(target))
            case tacky.JumpIfNotZero(tacky.Constant(const), target):
                if not is_zero(const):
                    optimized.append(tacky.Jump(target))
            case tacky.Truncate(tacky.Constant(const), dst):
                pass # TODO: this probably requires the symbol table to know the size of `dst`
            case tacky.SignExtend(tacky.Constant(const), dst):
                pass
            case tacky.ZeroExtend(tacky.Constant(const), dst):
                pass
            case tacky.DoubleToInt(tacky.Constant(const), dst):
                pass
            case tacky.DoubleToUInt(tacky.Constant(const), dst):
                pass
            case tacky.IntToDouble(tacky.Constant(const), dst):
                pass
            case tacky.UIntToDouble(tacky.Constant(const), dst):
                pass
            case _:
                optimized.append(instr)
    return optimized


def evaluate_unary_op(op: tacky.UnaryOp, const: tacky.Const):
    match op:
        case tacky.UnaryNegate():
            pass
        case tacky.UnaryInvert():
            pass
        case tacky.UnaryNot():
            pass
        case _:
            raise Exception(f'unhandled unary operator {op}')


def evaluate_binary_op(op: tacky.BinaryOp, left: tacky.Const, right: tacky.Const):
    match op:
        case tacky.BinaryAdd():
            pass
        case tacky.BinarySubtract():
            pass
        case tacky.BinaryMultiply():
            pass
        case tacky.BinaryDivide():
            pass
        case tacky.BinaryRemainder():
            pass
        case tacky.BitAnd():
            pass
        case tacky.BitOr():
            pass
        case tacky.BitXor():
            pass
        case tacky.ShiftLeft():
            pass
        case tacky.ShiftRight():
            pass
        case tacky.Less():
            pass
        case tacky.LessEqual():
            pass
        case tacky.Greater():
            pass
        case tacky.GreaterEqual():
            pass
        case tacky.Equals():
            pass
        case tacky.NotEquals():
            pass
        case _:
            raise Exception(f'unhandled binary operator {op}')


def is_zero(const: tacky.Const):
    ''' this should treat 0.0 and -0.0 as zero '''
    return const.value == 0


def make_control_flow_graph(body):
    # TODO
    return body


def unreachable_code_elimination(cfg):
    # TODO
    return cfg


def copy_propagation(cfg):
    # TODO
    return cfg


def dead_store_elimination(cfg):
    # TODO
    return cfg


def cfg_to_instructions(cfg):
    # TODO
    return cfg
