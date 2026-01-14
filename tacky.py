from collections import namedtuple


class Program(namedtuple('Program', ['top_level'])):
    def pretty_print(self):
        return '\n\n'.join(
            t.pretty_print()
            for t in self.top_level
        )


class StaticVariable(namedtuple('StaticVariable', ['name', 'is_global', 'var_type', 'init'])):
    def pretty_print(self):
        return str(self)


class Function(namedtuple('Function', ['name', 'is_global', 'params', 'body'])):
    def pretty_print(self):
        params = ', '.join(self.params)
        g = ''
        if self.is_global:
            g = 'global '
        return f'function {g}{self.name}({params}):\n' + \
            '\n'.join(instr.pretty_print() for instr in self.body)


##
## Instructions
##


class Instruction:
    def pretty_print(self):
        return '  ' + str(self)


class Return(Instruction, namedtuple('Constant', ['val'])):
    pass


class SignExtend(Instruction, namedtuple('SignExtend', ['src', 'dst'])):
    pass


class Truncate(Instruction, namedtuple('Truncate', ['src', 'dst'])):
    pass


class Unary(Instruction, namedtuple('Unary', ['unary_operator', 'src', 'dst'])):
    pass


class Binary(Instruction, namedtuple('Binary', ['operator', 'left', 'right', 'dst'])):
    pass


class Copy(Instruction, namedtuple('Copy', ['src', 'dst'])):
    pass


class Jump(Instruction, namedtuple('Jump', ['target'])):
    pass


class JumpIfZero(Instruction, namedtuple('JumpIfZero', ['condition', 'target'])):
    pass


class JumpIfNotZero(Instruction, namedtuple('JumpIfNotZero', ['condition', 'target'])):
    pass


class Label(Instruction, namedtuple('Label', ['name'])):
    pass


class Call(Instruction, namedtuple('Call', ['func_name', 'arg_vals', 'dst'])):
    pass


##
## Values
##


class Value:
    pass


class Constant(Value, namedtuple('Constant', ['const'])):
    pass


class Identifier(Value, namedtuple('Identifier', ['name'])):
    pass


##
## Constants
##

class Const:
    pass


class ConstInt(Const, namedtuple('ConstInt', ['value'])):
    pass


class ConstLong(Const, namedtuple('ConstLong', ['value'])):
    pass


##
## Unary operators
##


class UnaryOp:
    pass


class UnaryNegate(UnaryOp, namedtuple('UnaryNegate', [])):
    pass


class UnaryInvert(UnaryOp, namedtuple('UnaryInvert', [])):
    pass


class UnaryNot(UnaryOp, namedtuple('UnaryNot', [])):
    pass


##
## Binary operators
##


class BinaryOp:
    pass


class BinaryAdd(BinaryOp, namedtuple('BinaryAdd', [])):
    pass


class BinarySubtract(BinaryOp, namedtuple('BinarySubtract', [])):
    pass


class BinaryMultiply(BinaryOp, namedtuple('BinaryMultiply', [])):
    pass


class BinaryDivide(BinaryOp, namedtuple('BinaryDivide', [])):
    pass


class BinaryRemainder(BinaryOp, namedtuple('BinaryRemainder', [])):
    pass


class BitAnd(BinaryOp, namedtuple('BitAnd', [])):
    pass


class BitOr(BinaryOp, namedtuple('BitOr', [])):
    pass


class BitXor(BinaryOp, namedtuple('BitXor', [])):
    pass


class ShiftLeft(BinaryOp, namedtuple('ShiftLeft', [])):
    pass


class ShiftRight(BinaryOp, namedtuple('ShiftRight', [])):
    pass


class Less(BinaryOp, namedtuple('Less', [])):
    pass


class LessEqual(BinaryOp, namedtuple('LessEqual', [])):
    pass


class Greater(BinaryOp, namedtuple('Greater', [])):
    pass


class GreaterEqual(BinaryOp, namedtuple('GreaterEqual', [])):
    pass


class Equals(BinaryOp, namedtuple('Equals', [])):
    pass


class NotEquals(BinaryOp, namedtuple('NotEquals', [])):
    pass
