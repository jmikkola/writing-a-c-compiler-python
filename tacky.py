from collections import namedtuple


class Program(namedtuple('Program', ['function_definition'])):
    pass


class Function(namedtuple('Function', ['name', 'body'])):
    pass


class Instruction:
    pass


class Return(Instruction, namedtuple('Constant', ['val'])):
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


class Value:
    pass


class Constant(Value, namedtuple('Constant', ['value'])):
    pass


class Identifier(Value, namedtuple('Identifier', ['name'])):
    pass


class UnaryOp:
    pass


class UnaryNegate(UnaryOp, namedtuple('UnaryNegate', [])):
    pass


class UnaryInvert(UnaryOp, namedtuple('UnaryInvert', [])):
    pass


class UnaryNot(UnaryOp, namedtuple('UnaryNot', [])):
    pass


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
