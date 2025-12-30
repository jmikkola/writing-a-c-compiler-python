from collections import namedtuple


class Program(namedtuple('Program', ['function_definition'])):
    pass


class Function(namedtuple('Function', ['name', 'body'])):
    pass


class Instruction:
    pass


class Return(Instruction, namedtuple('Constant', ['val'])):
    pass


class Unary(Expression, namedtuple('Unary', ['unary_operator', 'src', 'dst'])):
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
