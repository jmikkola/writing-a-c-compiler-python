import math

import syntax
import tacky
import typeconversion


def optimize(ir: tacky.Program, args, symbols):
    optimizer = Optimizer(symbols)
    return optimizer.optimize(ir, args)


class Optimizer:
    def __init__(self, symbols):
        self.symbols = symbols

    def optimize(self, ir: tacky.Program, args) -> tacky.Program:
        optimized = []
        for decl in ir.top_level:
            match decl:
                case tacky.Function():
                    optimized.append(self.optimize_function(decl, args))
                case _:
                    optimized.append(decl)
        return tacky.Program(optimized)

    def optimize_function(self, f: tacky.Function, args):
        body = f.body
        if not body:
            return f

        while True:
            if args.fold_constants:
                post_constant_folding = self.constant_folding(body)
            else:
                post_constant_folding = body

            cfg = self.make_control_flow_graph(post_constant_folding)

            if args.eliminate_unreachable_code:
                cfg = self.unreachable_code_elimination(cfg)

            if args.propagate_copies:
                cfg = self.copy_propagation(cfg)

            if args.eliminate_dead_stores:
                cfg = self.dead_store_elimination(cfg)

            optimized_function_body = self.cfg_to_instructions(cfg)

            if optimized_function_body == body:
                break
            if not optimized_function_body:
                break

            body = optimized_function_body

        return tacky.Function(f.name, f.is_global, f.params, body)

    def constant_folding(self, body):
        optimized = []
        for instr in body:
            match instr:
                case tacky.Unary(op, tacky.Constant(const), dst):
                    dst_type = self.value_type(dst)
                    result = self.evaluate_unary_op(op, const, dst_type)
                    new_const = tacky.Constant(result)
                    optimized.append(tacky.Copy(new_const, dst))

                case tacky.Binary(op, tacky.Constant(left), tacky.Constant(right), dst):
                    dst_type = self.value_type(dst)
                    result = self.evaluate_binary_op(op, left, right, dst_type)
                    new_const = tacky.Constant(result)
                    optimized.append(tacky.Copy(new_const, dst))

                case tacky.JumpIfZero(tacky.Constant(const), target):
                    if is_zero(const):
                        optimized.append(tacky.Jump(target))

                case tacky.JumpIfNotZero(tacky.Constant(const), target):
                    if not is_zero(const):
                        optimized.append(tacky.Jump(target))

                case tacky.Truncate(tacky.Constant(const), dst):
                    dst_type = self.value_type(dst)
                    truncated = self.truncate_constant(const, dst_type)
                    optimized.append(tacky.Copy(truncated, dst))

                case tacky.SignExtend(tacky.Constant(const), dst):
                    dst_type = self.value_type(dst)
                    extended = self.sign_extend(const, dst_type)
                    optimized.append(tacky.Copy(extended, dst))

                case tacky.ZeroExtend(tacky.Constant(const), dst):
                    dst_type = self.value_type(dst)
                    extended = self.zero_extend(const, dst_type)
                    optimized.append(tacky.Copy(extended, dst))

                case tacky.DoubleToInt(tacky.Constant(const), dst):
                    dst_type = self.value_type(dst)
                    new_const = self.double_to_int(const, dst_type)
                    optimized.append(tacky.Copy(new_const, dst))

                case tacky.DoubleToUInt(tacky.Constant(const), dst):
                    dst_type = self.value_type(dst)
                    new_const = self.double_to_uint(const, dst_type)
                    optimized.append(tacky.Copy(new_const, dst))

                case tacky.IntToDouble(tacky.Constant(const), dst):
                    new_const = self.int_to_double(const)
                    optimized.append(tacky.Copy(new_const, dst))

                case tacky.UIntToDouble(tacky.Constant(const), dst):
                    new_const = self.uint_to_double(const)
                    optimized.append(tacky.Copy(new_const, dst))

                case _:
                    optimized.append(instr)
        return optimized

    def value_type(self, value: tacky.Value) -> syntax.Type:
        match value:
            case tacky.Constant(tacky.ConstChar(value)):
                return syntax.Char()
            case tacky.Constant(tacky.ConstInt(value)):
                return syntax.Int()
            case tacky.Constant(tacky.ConstLong(value)):
                return syntax.Long()
            case tacky.Constant(tacky.ConstUInt(value)):
                return syntax.UInt()
            case tacky.Constant(tacky.ConstULong(value)):
                return syntax.ULong()
            case tacky.Constant(tacky.ConstDouble(value)):
                return syntax.Double()
            case tacky.Constant(c):
                raise Exception(f'unhandled type of constant {c}')
            case tacky.Identifier(name):
                return self.symbols[name].type
            case _:
                raise Exception(f'unexpected value {value}')

    def truncate_constant(self, const: tacky.Const, dst_type: syntax.Type) -> tacky.Constant:
        truncated = const.value & make_mask(dst_type)
        result = self.as_type(truncated, dst_type)
        return tacky.Constant(result)

    def sign_extend(self, const: tacky.Const, dst_type: syntax.Type) -> tacky.Constant:
        # no actual change needed in the python representation
        extended = const.value
        result = self.as_type(extended, dst_type)
        return tacky.Constant(result)

    def zero_extend(self, const: tacky.Const, dst_type: syntax.Type) -> tacky.Constant:
        src_type = self.value_type(tacky.Constant(const))
        extended = const.value & make_mask(src_type)
        result = self.as_type(extended, dst_type)
        return tacky.Constant(result)

    def double_to_int(self, const: tacky.Const, dst_type: syntax.Type) -> tacky.Constant:
        d = const.value
        # Doesn't handle NaN or out of bounds values
        value = int(d)
        result = self.as_type(value, dst_type)
        return tacky.Constant(result)

    def double_to_uint(self, const: tacky.Const, dst_type: syntax.Type) -> tacky.Constant:
        d = const.value
        if d < 0:
            d = 0
        # Doesn't handle NaN or out of bounds values
        value = int(d)
        result = self.as_type(value, dst_type)
        return tacky.Constant(result)

    def int_to_double(self, const: tacky.Const) -> tacky.Constant:
        value = float(const.value)
        result = tacky.ConstDouble(value)
        return tacky.Constant(result)

    def uint_to_double(self, const: tacky.Const) -> tacky.Constant:
        value = float(const.value)
        result = tacky.ConstDouble(value)
        return tacky.Constant(result)

    def as_type(self, value, ctype: syntax.Type) -> tacky.Const:
        match ctype:
            case syntax.Char() | syntax.SChar() | syntax.UChar():
                return tacky.ConstChar(value & mask_for_bytes(1))
            case syntax.Int():
                return tacky.ConstInt(value & mask_for_bytes(4))
            case syntax.Long():
                return tacky.ConstLong(value & mask_for_bytes(8))
            case syntax.UInt():
                return tacky.ConstUInt(value & mask_for_bytes(4))
            case syntax.ULong():
                return tacky.ConstULong(value & mask_for_bytes(8))
            case syntax.Double():
                return tacky.ConstDouble(value)
            case syntax.Pointer():
                return tacky.ConstULong(value & mask_for_bytes(8))
            case _:
                raise Exception(f'unhandled type for as_type: {ctype}')

    def evaluate_unary_op(self, op: tacky.UnaryOp, const: tacky.Const, dst_type: syntax.Type):
        match op:
            case tacky.UnaryNegate():
                value = -const.value
            case tacky.UnaryInvert():
                value = const.value ^ make_mask(dst_type)
            case tacky.UnaryNot():
                value = int(not const.value)
            case _:
                raise Exception(f'unhandled unary operator {op}')
        return self.as_type(value, dst_type)

    def evaluate_binary_op(self, op: tacky.BinaryOp, left: tacky.Const, right: tacky.Const, dst_type: syntax.Type):
        match op:
            case tacky.BinaryAdd():
                value = left.value + right.value
            case tacky.BinarySubtract():
                value = left.value - right.value
            case tacky.BinaryMultiply():
                value = left.value * right.value
            case tacky.BinaryDivide():
              if dst_type == syntax.Double():
                  if is_zero(right):
                      value = float('nan')
                  else:
                      value = left.value / right.value
              else:
                  if is_zero(right):
                      value = 0
                  else:
                      value = left.value // right.value
            case tacky.BinaryRemainder():
                if is_zero(right):
                    value = 0
                else:
                    value = left.value % right.value
            case tacky.BitAnd():
                value = left.value & right.value
            case tacky.BitOr():
                value = left.value | right.value
            case tacky.BitXor():
                value = left.value ^ right.value
            case tacky.ShiftLeft():
                value = left.value << right.value
            case tacky.ShiftRight():
                value = left.value >> right.value
            case tacky.Less():
                value = int(left.value < right.value)
            case tacky.LessEqual():
                value = int(left.value <= right.value)
            case tacky.Greater():
                value = int(left.value > right.value)
            case tacky.GreaterEqual():
                value = int(left.value >= right.value)
            case tacky.Equals():
                value = int(left.value == right.value)
            case tacky.NotEquals():
                value = int(left.value != right.value)
            case _:
                raise Exception(f'unhandled binary operator {op}')
        return self.as_type(value, dst_type)

    def make_control_flow_graph(self, body):
        # TODO
        return body

    def unreachable_code_elimination(self, cfg):
        # TODO
        return cfg

    def copy_propagation(self, cfg):
        # TODO
        return cfg

    def dead_store_elimination(self, cfg):
        # TODO
        return cfg

    def cfg_to_instructions(self, cfg):
        # TODO
        return cfg


def is_zero(const: tacky.Const):
    ''' this should treat 0.0 and -0.0 as zero '''
    return const.value in (0, 0.0, -0.0)


def make_mask(t: syntax.Type):
    size_bytes = typeconversion.type_size(t, None)
    return mask_for_bytes(size_bytes)


def mask_for_bytes(size_bytes):
    n_bits = size_bytes * 8
    return (1 << n_bits) - 1
