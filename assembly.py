from collections import namedtuple
from enum import Enum


class AssemblyType(Enum):
    Byte = 1
    Longword = 4
    Quadword = 8

    def bytes(self):
        if self == self.Byte:
            return 1
        if self == self.Longword:
            return 4
        if self == self.Quadword:
            return 8
        assert(False)


class AsmSymbol:
    pass


class ObjEntry(AsmSymbol, namedtuple('ObjEntry', ['assembly_type', 'is_static'])):
    pass


class FunEntry(AsmSymbol, namedtuple('FunEntry', ['is_defined'])):
    pass


class Program(namedtuple('Program', ['top_level'])):
    def pretty_print(self):
        return '\n\n'.join(d.pretty_print() for d in self.top_level)


class StaticVariable(namedtuple('StaticVariable', ['name', 'is_global', 'alignment', 'init'])):
    def pretty_print(self):
        return str(self)


class Function(namedtuple('Function', ['name', 'is_global', 'instructions'])):
    def pretty_print(self):
        return f'function {self.name}():\n' + \
            '\n'.join(i.pretty_print() for i in self.instructions)


class Instruction:
    def pretty_print(self):
        return '  ' + str(self)


class Ret(Instruction, namedtuple('Ret', [])):
    pass


class Mov(Instruction, namedtuple('Mov', ['assembly_type', 'src', 'dst'])):
    pass


class Movsx(Instruction, namedtuple('Movsx', ['src', 'dst'])):
    pass


class MovZeroExtend(Instruction, namedtuple('MovZeroExtend', ['src', 'dst'])):
    pass


class Push(Instruction, namedtuple('Push', ['operand'])):
    pass


class Call(Instruction, namedtuple('Call', ['identifier'])):
    pass


class Unary(Instruction, namedtuple('Unary', ['unary_operator', 'assembly_type', 'operand'])):
    pass


class Binary(Instruction, namedtuple('Binary', ['binary_operator', 'assembly_type', 'src', 'dst'])):
    pass


class Cmp(Instruction, namedtuple('Cmp', ['assembly_type', 'left', 'right'])):
    pass


class Idiv(Instruction, namedtuple('Idiv', ['assembly_type', 'operand'])):
    pass


class Div(Instruction, namedtuple('Div', ['assembly_type', 'operand'])):
    pass


class Cdq(Instruction, namedtuple('Cdq', ['assembly_type'])):
    pass


class Jmp(Instruction, namedtuple('Jmp', ['label'])):
    pass


class JmpCC(Instruction, namedtuple('JmpCC', ['cond_code', 'label'])):
    ''' cond_code can be one of E, NE, G, GE, L, LE, A, AE, B, or BE '''
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
    def __eq__(self, o):
        return str(self) == str(o)


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


class ShiftRightLogical(BinaryOperator, namedtuple('ShiftRightLogical', [])):
    pass


class Operand:
    pass


class Immediate(Operand, namedtuple('Immediate', ['value'])):
    pass


class Register(Operand, namedtuple('Register', ['reg'])):
    ''' reg can be 'AX', 'CX', 'DX', 'DI', 'SI', 'SP', 'R8', 'R9', 'R10', or 'R11' '''
    pass


class Pseudo(Operand, namedtuple('Pseudo', ['name'])):
    pass


class Stack(Operand, namedtuple('Stack', ['offset'])):
    pass


class Data(Operand, namedtuple('Data', ['name'])):
    pass
