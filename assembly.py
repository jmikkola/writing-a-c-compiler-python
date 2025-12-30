from collections import namedtuple


class Program(namedtuple('Program', ['function_definition'])):
    pass


class Function(namedtuple('Function', ['name', 'instructions'])):
    pass


class Instruction:
    pass


class Ret(Instruction, namedtuple('Ret', [])):
    pass


class Mov(Instruction, namedtuple('Mov', ['src', 'dst'])):
    pass


class AllocateStack(Instruction, namedtuple('AllocateStack', ['bytes'])):
    pass


class Unary(Instruction, namedtuple('Unary', ['unary_operator', 'operand'])):
    pass


class UnaryOperator:
    pass


class Not(UnaryOperator, namedtuple('Not', [])):
    pass


class Neg(UnaryOperator, namedtuple('Neg', [])):
    pass


class Operand:
    pass


class Immediate(Operand, namedtuple('Immediate', ['value'])):
    pass


class Register(Operand, namedtuple('Register', ['reg'])):
    ''' reg can be 'AX' or 'R10' '''
    pass


class Pseudo(Operand, namedtuple('Pseudo', ['name'])):
    pass


class Stack(Operand, namedtuple('Stack', ['offset'])):
    pass
