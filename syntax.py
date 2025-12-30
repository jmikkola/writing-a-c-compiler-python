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
