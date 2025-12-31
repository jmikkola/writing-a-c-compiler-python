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


class Binary(Instruction, namedtuple('Binary', ['binary_operator', 'src', 'dst'])):
    pass


class BinaryOperator:
    pass


class Add(BinaryOperator, namedtuple('Add', [])):
    pass


class Sub(BinaryOperator, namedtuple('Sub', [])):
    pass


class Mult(BinaryOperator, namedtuple('Mult', [])):
    pass


class BitAnd(BinaryOperator, namedtuple('BitAnd', [])):
    pass


class BitOr(BinaryOperator, namedtuple('BitOr', [])):
    pass


class BitXor(BinaryOperator, namedtuple('BitXor', [])):
    pass


class ShiftLeft(BinaryOperator, namedtuple('ShiftLeft', [])):
    pass


class ShiftRight(BinaryOperator, namedtuple('ShiftRight', [])):
    pass


class Idiv(Instruction, namedtuple('Idiv', ['operand'])):
    pass


class Cdq(Instruction, namedtuple('Cdq', [])):
    pass


class Operand:
    pass


class Immediate(Operand, namedtuple('Immediate', ['value'])):
    pass


class Register(Operand, namedtuple('Register', ['reg'])):
    ''' reg can be 'AX', 'CX', 'DX', 'R10', or 'R11' '''
    pass


class Pseudo(Operand, namedtuple('Pseudo', ['name'])):
    pass


class Stack(Operand, namedtuple('Stack', ['offset'])):
    pass
