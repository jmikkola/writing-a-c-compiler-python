from collections import namedtuple
from dataclasses import dataclass
from typing import TypeAlias


def is_callee_saved(register_name: str):
    return register_name in ('BX', 'R12', 'R13', 'R14', 'R15')


@dataclass(frozen=True)
class Byte:
    def bytes(self):
        return 1


@dataclass
class Longword:
    def bytes(self):
        return 4


@dataclass
class Quadword:
    def bytes(self):
        return 8


@dataclass
class Double:
    def bytes(self):
        return 8


@dataclass
class ByteArray:
    size: int
    alignment: int

    def bytes(self):
        return self.size


AssemblyType: TypeAlias = Byte | Longword | Quadword | Double | ByteArray


class AsmSymbol:
    ''' this is the type used in the asm_symbols table '''
    pass


class ObjEntry(AsmSymbol, namedtuple('ObjEntry', ['assembly_type', 'is_static', 'is_constant'])):
    pass


class FunEntry(AsmSymbol, namedtuple('FunEntry', ['is_defined', 'return_on_stack'])):
    pass


class Program(namedtuple('Program', ['top_level'])):
    def pretty_print(self):
        return '\n\n'.join(d.pretty_print() for d in self.top_level)


class StaticVariable(namedtuple('StaticVariable', ['name', 'is_global', 'alignment', 'inits'])):
    def pretty_print(self):
        return str(self)


class StaticConstant(namedtuple('StaticConstant', ['name', 'alignment', 'init'])):
    def pretty_print(self):
        return str(self)


class Function(namedtuple('Function', ['name', 'is_global', 'instructions'])):
    def pretty_print(self):
        return f'function {self.name}():\n' + \
            '\n'.join(i.pretty_print() for i in self.instructions)


class Instruction:
    def pretty_print(self):
        return '  ' + str(self)

    def operands(self):
        raise NotImplementedError()

    def used(self, fnclass):
        ''' the list of operands that this instruction reads '''
        raise NotImplementedError()

    def used_registers(self, fnclass):
        ''' the list of registers and pseudoregisters that this instruction reads '''
        used = []
        for operand in self.used(fnclass):
            match operand:
                case Immediate():
                    pass
                case Register():
                    used.append(operand)
                case Indexed(base, index, _scale):
                    used.append(Register(base))
                    used.append(Register(index))
                case Pseudo():
                    used.append(operand)
                case PseudoMem():
                    used.append(operand)
                case Memory(reg, _offset):
                    used.append(Register(reg))
                case Data():
                    pass
        return used

    def updated(self):
        ''' the list of operands that this instruction updates '''
        raise NotImplementedError()


class Ret(Instruction, namedtuple('Ret', [])):
    def operands(self):
        return []

    def used(self, fnclass):
        return []

    def updated(self):
        return []


class Mov(Instruction, namedtuple('Mov', ['assembly_type', 'src', 'dst'])):
    def operands(self):
        return [self.src, self.dst]

    def used(self, fnclass):
        return [self.src]

    def updated(self):
        return [self.dst]


class Movsx(Instruction, namedtuple('Movsx', ['src_type', 'dst_type', 'src', 'dst'])):
    def operands(self):
        return [self.src, self.dst]

    def used(self, fnclass):
        return [self.src]

    def updated(self):
        return [self.dst]


class MovZeroExtend(Instruction, namedtuple('MovZeroExtend', ['src_type', 'dst_type', 'src', 'dst'])):
    # src_type and dst_type are assembly.AssemblyType values
    def operands(self):
        return [self.src, self.dst]

    def used(self, fnclass):
        return [self.src]

    def updated(self):
        return [self.dst]


class Lea(Instruction, namedtuple('Lea', ['src', 'dst'])):
    def operands(self):
        return [self.src, self.dst]

    def used(self, fnclass):
        return [self.src]

    def updated(self):
        return [self.dst]


class Push(Instruction, namedtuple('Push', ['operand'])):
    def operand(self):
        return [self.operand]

    def used(self, fnclass):
        return [self.operand]

    def updated(self):
        return []


class Pop(Instruction, namedtuple('Pop', ['reg'])):
    def operands(self):
        return []

    def used(self, fnclass):
        return []

    def updated(self):
        return [self.reg]


class Call(Instruction, namedtuple('Call', ['identifier'])):
    def operands(self):
        return []

    def used(self, fnclass):
        (arg_registers, _return_registers) = fnclass[self.identifier]
        return arg_registers

    def updated(self):
        xmm_registers = [Register(f'XMM{n}') for n in range(14)]
        int_registers = [
            Register('DI'),
            Register('SI'),
            Register('DX'),
            Register('CX'),
            Register('R8'),
            Register('R9'),
            Register('AX'),
        ]
        return int_registers + xmm_registers


class Unary(Instruction, namedtuple('Unary', ['unary_operator', 'assembly_type', 'operand'])):
    def operands(self):
        return [self.operand]

    def used(self, fnclass):
        return [self.operand]

    def updated(self):
        return [self.operand]


class Binary(Instruction, namedtuple('Binary', ['binary_operator', 'assembly_type', 'src', 'dst'])):
    def operands(self):
        return [self.src, self.dst]

    def used(self, fnclass):
        return [self.src, self.dst]

    def updated(self):
        return [self.dst]


class Cmp(Instruction, namedtuple('Cmp', ['assembly_type', 'left', 'right'])):
    def operands(self):
        return [self.left, self.right]

    def used(self, fnclass):
        return [self.left, self.right]

    def updated(self):
        return []


class Idiv(Instruction, namedtuple('Idiv', ['assembly_type', 'operand'])):
    def operands(self):
        return [self.operand]

    def used(self, fnclass):
        return [self.operand, Register('AX'), Register('DX')]

    def updated(self):
        return [Register('AX'), Register('DX')]


class Div(Instruction, namedtuple('Div', ['assembly_type', 'operand'])):
    def operands(self):
        return [self.operand]

    def used(self, fnclass):
        return [self.operand, Register('AX'), Register('DX')]

    def updated(self):
        return [Register('AX'), Register('DX')]


class Cdq(Instruction, namedtuple('Cdq', ['assembly_type'])):
    def operands(self):
        return []

    def used(self, fnclass):
        return [Register('AX')]

    def updated(self):
        return [Register('DX')]


class Jmp(Instruction, namedtuple('Jmp', ['label'])):
    def operands(self):
        return []

    def used(self, fnclass):
        return []

    def updated(self):
        return []


class JmpCC(Instruction, namedtuple('JmpCC', ['cond_code', 'label'])):
    ''' cond_code can be one of E, NE, G, GE, L, LE, A, AE, B, or BE '''
    def operands(self):
        return []

    def used(self, fnclass):
        return []

    def updated(self):
        return []


class SetCC(Instruction, namedtuple('SetCC', ['cond_code', 'operand'])):
    def operands(self):
        return [self.operand]

    def used(self, fnclass):
        return [self.operand]

    def updated(self):
        return []


class Label(Instruction, namedtuple('Label', ['name'])):
    def operands(self):
        return []

    def used(self, fnclass):
        return []

    def updated(self):
        return []


class Cvttsd2si(Instruction, namedtuple('Cvttsd2si', ['assembly_type', 'src', 'dst'])):
    def operands(self):
        return [self.src, self.dst]

    def used(self, fnclass):
        return [self.src]

    def updated(self):
        return [self.dst]


class Cvtsi2sd(Instruction, namedtuple('Cvtsi2sd', ['assembly_type', 'src', 'dst'])):
    def operands(self):
        return [self.src, self.dst]

    def used(self, fnclass):
        return [self.src]

    def updated(self):
        return [self.dst]


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


class DivDouble(BinaryOperator, namedtuple('DivDouble', [])):
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
    def __eq__(self, other):
        return str(self) == str(other)

    def __ne__(self, other):
        return not (self == other)

    def __hash__(self):
        return hash(str(self))


class Immediate(Operand, namedtuple('Immediate', ['value'])):
    pass


class Register(Operand, namedtuple('Register', ['reg'])):
    ''' reg can be 'AX', 'CX', 'DX', 'DI', 'SI', 'SP', 'R8' through 'R15',
    or any XMM register. '''
    pass


class Indexed(Operand, namedtuple('Indexed', ['base', 'index', 'scale'])):
    '''
    base: a register
    index: a register
    scale: int (a small power of 2)
    '''
    pass


class Pseudo(Operand, namedtuple('Pseudo', ['name'])):
    def add_offset(self, offset):
        # Convert to PseudoMem to get the ability to handle offsets
        return PseudoMem(self.name, offset)


class PseudoMem(Operand, namedtuple('PseudoMem', ['name', 'offset'])):
    def add_offset(self, offset):
        return PseudoMem(self.name, self.offset + offset)


class Memory(Operand, namedtuple('Memory', ['reg', 'offset'])):
    def add_offset(self, offset):
        return Memory(self.reg, self.offset + offset)


class Data(Operand, namedtuple('Data', ['name', 'offset'])):
    pass
