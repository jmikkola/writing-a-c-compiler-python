from collections import namedtuple


def indent(lines):
    return ['  ' + line for line in lines]


class Program(namedtuple('Program', ['function_definition'])):
    def pretty_print(self):
        return self.function_definition.pretty_print()


class Function(namedtuple('Function', ['name', 'body'])):
    def pretty_print(self):
        body = [line for bi in self.body for line in bi.pretty_print()]
        return [f'function {self.name}():'] + indent(body)


class BlockItem:
    def pretty_print(self):
        return [str(self)]


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


class IfStatement(Statement, namedtuple('IfStatement', ['condition', 't', 'e'])):
    pass


class Goto(Statement, namedtuple('Goto', ['label'])):
    pass


class LabeledStmt(Statement, namedtuple('LabeledExpr', ['label', 'stmt'])):
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


class Conditional(Expression, namedtuple('Conditional', ['condition', 't', 'e'])):
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
