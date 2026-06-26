from collections import namedtuple


class Program(namedtuple('Program', ['top_level'])):
    def pretty_print(self):
        return '\n\n'.join(
            t.pretty_print()
            for t in self.top_level
        )


class StaticVariable(namedtuple('StaticVariable', ['name', 'is_global', 'var_type', 'inits'])):
    def pretty_print(self):
        return str(self)


class StaticConstant(namedtuple('StaticConstant', ['name', 'const_type', 'init'])):
    def pretty_print(self):
        return str(self)


class Function(namedtuple('Function', ['name', 'is_global', 'params', 'body'])):
    def pretty_print(self):
        params = ', '.join(self.params)
        g = ''
        if self.is_global:
            g = 'global '
        return f'function {g}{self.name}({params}):\n' + \
            '\n'.join(instr.pretty_print() for instr in self.body)


##
## Instructions
##


class Instruction:
    def pretty_print(self):
        return '  ' + str(self)

    def __eq__(self, other):
        # Super lazy
        return str(self) == str(other)

    def __hash__(self):
        return hash(str(self))


class Return(Instruction, namedtuple('Return', ['val'])):
    # val is optional
    def get_dst(self):
        return None


class SignExtend(Instruction, namedtuple('SignExtend', ['src', 'dst'])):
    def get_dst(self):
        return self.dst


class Truncate(Instruction, namedtuple('Truncate', ['src', 'dst'])):
    def get_dst(self):
        return self.dst


class ZeroExtend(Instruction, namedtuple('ZeroExtend', ['src', 'dst'])):
    def get_dst(self):
        return self.dst


class DoubleToInt(Instruction, namedtuple('DoubleToInt', ['src', 'dst'])):
    def get_dst(self):
        return self.dst


class DoubleToUInt(Instruction, namedtuple('DoubleToUInt', ['src', 'dst'])):
    def get_dst(self):
        return self.dst


class IntToDouble(Instruction, namedtuple('IntToDouble', ['src', 'dst'])):
    def get_dst(self):
        return self.dst


class UIntToDouble(Instruction, namedtuple('UIntToDouble', ['src', 'dst'])):
    def get_dst(self):
        return self.dst


class Unary(Instruction, namedtuple('Unary', ['unary_operator', 'src', 'dst'])):
    def get_dst(self):
        return self.dst


class Binary(Instruction, namedtuple('Binary', ['operator', 'left', 'right', 'dst'])):
    def get_dst(self):
        return self.dst


class Copy(Instruction, namedtuple('Copy', ['src', 'dst'])):
    def get_dst(self):
        return self.dst


class GetAddress(Instruction, namedtuple('GetAddress', ['src', 'dst'])):
    def get_dst(self):
        return self.dst


class Load(Instruction, namedtuple('Load', ['src_ptr', 'dst'])):
    def get_dst(self):
        return self.dst


class Store(Instruction, namedtuple('Store', ['src', 'dst_ptr'])):
    def get_dst(self):
        return self.dst_ptr


class AddPtr(Instruction, namedtuple('AddPtr', ['ptr', 'index', 'scale', 'dst'])):
    '''
    ptr: value
    index: value
    scale: int
    dst: value
    '''
    def get_dst(self):
        return self.dst


class CopyToOffset(Instruction, namedtuple('CopyToOffset', ['src', 'dst', 'offset'])):
    def get_dst(self):
        return self.dst


class CopyFromOffset(Instruction, namedtuple('CopyFromOffset', ['src', 'offset', 'dst'])):
    def get_dst(self):
        return self.dst


class Jump(Instruction, namedtuple('Jump', ['target'])):
    def get_dst(self):
        return None


class JumpIfZero(Instruction, namedtuple('JumpIfZero', ['condition', 'target'])):
    def get_dst(self):
        return None


class JumpIfNotZero(Instruction, namedtuple('JumpIfNotZero', ['condition', 'target'])):
    def get_dst(self):
        return None


class Label(Instruction, namedtuple('Label', ['name'])):
    def get_dst(self):
        return None


class Call(Instruction, namedtuple('Call', ['func_name', 'arg_vals', 'dst'])):
    # dst is optional
    def get_dst(self):
        return self.dst


##
## Values
##


class Value:
    pass


class Constant(Value, namedtuple('Constant', ['const'])):
    def __new__(cls, const):
        assert(isinstance(const, Const))
        return super().__new__(cls, const)


class Identifier(Value, namedtuple('Identifier', ['name'])):
    pass


##
## Constants
##

class Const:
    pass


class ConstChar(Const, namedtuple('ConstChar', ['value'])):
    pass


class ConstUChar(Const, namedtuple('ConstUChar', ['value'])):
    pass


class ConstInt(Const, namedtuple('ConstInt', ['value'])):
    pass


class ConstLong(Const, namedtuple('ConstLong', ['value'])):
    pass


class ConstUInt(Const, namedtuple('ConstUInt', ['value'])):
    pass


class ConstULong(Const, namedtuple('ConstULong', ['value'])):
    pass


class ConstDouble(Const, namedtuple('ConstDouble', ['value'])):
    pass


##
## Unary operators
##


class UnaryOp:
    def __eq__(self, other):
        # Super lazy
        return str(self) == str(other)

    def __ne__(self, other):
        return not (self == other)


class UnaryNegate(UnaryOp, namedtuple('UnaryNegate', [])):
    pass


class UnaryInvert(UnaryOp, namedtuple('UnaryInvert', [])):
    pass


class UnaryNot(UnaryOp, namedtuple('UnaryNot', [])):
    pass


##
## Binary operators
##


class BinaryOp:
    def __eq__(self, other):
        # Super lazy
        return str(self) == str(other)

    def __ne__(self, other):
        return not (self == other)


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
