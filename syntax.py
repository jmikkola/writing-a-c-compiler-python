from collections import namedtuple


class Program(namedtuple('Program', ['function_definition'])):
    pass


class Function(namedtuple('Function', ['name', 'body'])):
    pass


class Statement:
    pass


class Return(Statement, namedtuple('Return', ['expr'])):
    pass


class Expression:
    pass


class Constant(Expression, namedtuple('Constant', ['value'])):
    pass


class Unary(Expression, namedtuple('Unary', ['operator', 'expr'])):
    pass


class UnaryOp:
    pass


class UnaryNegate(UnaryOp, namedtuple('UnaryNegate', [])):
    ''' -n '''
    pass


class UnaryInvert(UnaryOp, namedtuple('UnaryInvert', [])):
    ''' ~n '''
    pass


class UnaryNot(UnaryOp, namedtuple('UnaryNot', [])):
    ''' !n '''
    pass


class Binary(Expression, namedtuple('Binary', ['operator', 'left', 'right'])):
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


class BinaryAnd(BinaryOp, namedtuple('BinaryAnd', [])):
    pass


class BinaryOr(BinaryOp, namedtuple('BinaryOr', [])):
    pass
