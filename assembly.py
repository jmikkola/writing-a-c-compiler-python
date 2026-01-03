from collections import namedtuple


class Program(namedtuple('Program', ['functions'])):
    def pretty_print(self):
        return '\n\n'.join(f.pretty_print() for f in self.functions)


class Function(namedtuple('Function', ['name', 'instructions'])):
    def pretty_print(self):
        return f'function {self.name}():\n' + \
            '\n'.join(i.pretty_print() for i in self.instructions)


class Instruction:
    def pretty_print(self):
        return '  ' + str(self)


class Ret(Instruction, namedtuple('Ret', [])):
    pass


class Mov(Instruction, namedtuple('Mov', ['src', 'dst'])):
    pass


class AllocateStack(Instruction, namedtuple('AllocateStack', ['bytes'])):
    pass


class Unary(Instruction, namedtuple('Unary', ['unary_operator', 'operand'])):
    pass


class Binary(Instruction, namedtuple('Binary', ['binary_operator', 'src', 'dst'])):
    pass


class Cmp(Instruction, namedtuple('Cmp', ['left', 'right'])):
    pass


class Idiv(Instruction, namedtuple('Idiv', ['operand'])):
    pass


class Cdq(Instruction, namedtuple('Cdq', [])):
    pass


class Jmp(Instruction, namedtuple('Jmp', ['label'])):
    pass


class JmpCC(Instruction, namedtuple('JmpCC', ['cond_code', 'label'])):
    ''' cond_code can be one of E, NE, G, GE, L, or LE '''
    pass


class SetCC(Instruction, namedtuple('SetCC', ['cond_code', 'operand'])):
    pass


class Label(Instruction, namedtuple('Label', ['name'])):
    pass


class UnaryOperator:
    pass


class Not(UnaryOperator, namedtuple('Not', [])):
    pass


class Neg(UnaryOperator, namedtuple('Neg', [])):
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
