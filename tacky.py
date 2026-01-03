from collections import namedtuple


class Program(namedtuple('Program', ['functions'])):
    def pretty_print(self):
        return '\n\n'.join(
            f.pretty_print()
            for f in self.functions
        )


class Function(namedtuple('Function', ['name', 'params', 'body'])):
    def pretty_print(self):
        params = ', '.join(self.params)
        return f'function {self.name}({params}):\n' + \
            '\n'.join(instr.pretty_print() for instr in self.body)


##
## Instructions
##


class Instruction:
    def pretty_print(self):
        return '  ' + str(self)


class Return(Instruction, namedtuple('Constant', ['val'])):
    pass


class Unary(Instruction, namedtuple('Unary', ['unary_operator', 'src', 'dst'])):
    pass


class Binary(Instruction, namedtuple('Binary', ['operator', 'left', 'right', 'dst'])):
    pass


class Copy(Instruction, namedtuple('Copy', ['src', 'dst'])):
    pass


class Jump(Instruction, namedtuple('Jump', ['target'])):
    pass


class JumpIfZero(Instruction, namedtuple('JumpIfZero', ['condition', 'target'])):
    pass


class JumpIfNotZero(Instruction, namedtuple('JumpIfNotZero', ['condition', 'target'])):
    pass


class Label(Instruction, namedtuple('Label', ['name'])):
    pass


class Call(Instruction, namedtuple('Call', ['func_name', 'arg_vals', 'dst'])):
    pass


##
## Values
##


class Value:
    pass


class Constant(Value, namedtuple('Constant', ['value'])):
    pass


class Identifier(Value, namedtuple('Identifier', ['name'])):
    pass


##
## Unary operators
##


class UnaryOp:
    pass


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
