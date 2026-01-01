from collections import namedtuple


class Program(namedtuple('Program', ['function_definition'])):
    pass


class Function(namedtuple('Function', ['name', 'body'])):
    pass


class BlockItem:
    pass


class Declaration(BlockItem, namedtuple('Declaration', ['name', 'init'])):
    pass

##
## Statements
##

class Statement(BlockItem):
    pass


class Return(Statement, namedtuple('Return', ['expr'])):
    pass


class ExprStmt(Statement, namedtuple('ExprStmt', ['expr'])):
    pass


class NullStatement(Statement, namedtuple('NullStatement', [])):
    pass

##
## Expressions
##

class Expression:
    pass


class Constant(Expression, namedtuple('Constant', ['value'])):
    pass


class Variable(Expression, namedtuple('Variable', ['name'])):
    pass


class Unary(Expression, namedtuple('Unary', ['operator', 'expr'])):
    pass


class Postfix(Expression, namedtuple('Postfix', ['expr', 'operator'])):
    pass


class Binary(Expression, namedtuple('Binary', ['operator', 'left', 'right'])):
    pass


class Assignment(Expression, namedtuple('Assignment', ['lhs', 'rhs', 'op'])):
    pass

##
## Unary ops
##

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


class UnaryIncrement(UnaryOp, namedtuple('UnaryIncrement', [])):
    ''' ++n or n++ '''
    pass


class UnaryDecrement(UnaryOp, namedtuple('UnaryDecrement', [])):
    ''' --n or n-- '''
    pass

##
## Binary Ops
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


class BinaryAnd(BinaryOp, namedtuple('BinaryAnd', [])):
    pass


class BinaryOr(BinaryOp, namedtuple('BinaryOr', [])):
    pass
