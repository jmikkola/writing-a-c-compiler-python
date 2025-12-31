import syntax
import tacky


def to_ir(syntax: syntax.Program) -> tacky.Program:
    tt = ToTacky(syntax)
    return tt.convert()


class ToTacky:
    def __init__(self, syntax):
        self.syntax = syntax
        self.n_temp_vars = 0

    def new_temp_var(self):
        name = f'tmp.{self.n_temp_vars}'
        self.n_temp_vars += 1
        return tacky.Identifier(name)

    def convert(self):
        function = self.convert_function(self.syntax.function_definition)
        return tacky.Program(function_definition=function)

    def convert_function(self, function: syntax.Function) -> tacky.Function:
        instructions = self.convert_instructions(function.body)
        return tacky.Function(name=function.name, body=instructions)

    def convert_instructions(self, body: syntax.Statement) -> list:
        match body:
            case syntax.Return(expr=_):
                return self.convert_return(body)
            case _:
                raise Exception(f'unhandled statement type, {body}')

    def convert_return(self, stmt: syntax.Return) -> list:
        instructions, val = self.convert_expression(stmt.expr)
        return instructions + [tacky.Return(val)]

    def convert_expression(self, expr: syntax.Expression) -> (list, tacky.Value):
        ''' returns (instructions, result value) '''
        match expr:
            case syntax.Constant(value):
                return ([], tacky.Constant(value))
            case syntax.Unary(operator, expr=inner):
                instructions, val = self.convert_expression(inner)
                op = self.convert_unary_op(operator)
                result_var = self.new_temp_var()
                instruction = tacky.Unary(unary_operator=op, src=val, dst=result_var)
                return (instructions + [instruction], result_var)
            case syntax.Binary(operator, left, right):
                instructions_left, val_left = self.convert_expression(left)
                instructions_right, val_right = self.convert_expression(right)
                op = self.convert_binary_op(operator)
                result_var = self.new_temp_var()
                instruction = tacky.Binary(operator=op, left=val_left, right=val_right, dst=result_var)
                instructions = instructions_left + instructions_right + [instruction]
                return (instructions, result_var)
            case _:
                raise Exception(f'unhandled expression type, {expr}')

    def convert_binary_op(self, op: syntax.BinaryOp) -> tacky.BinaryOp:
        match op:
            case syntax.BinaryAdd():
                return tacky.BinaryAdd()
            case syntax.BinarySubtract():
                return tacky.BinarySubtract()
            case syntax.BinaryMultiply():
                return tacky.BinaryMultiply()
            case syntax.BinaryDivide():
                return tacky.BinaryDivide()
            case syntax.BinaryRemainder():
                return tacky.BinaryRemainder()
            case syntax.BitOr():
                return tacky.BitOr()
            case syntax.BitAnd():
                return tacky.BitAnd()
            case syntax.BitXor():
                return tacky.BitXor()
            case syntax.ShiftLeft():
                return tacky.ShiftLeft()
            case syntax.ShiftRight():
                return tacky.ShiftRight()
            case _:
                raise Exception(f'unhandled binary operator {op}')

    def convert_unary_op(self, op: syntax.UnaryOp) -> tacky.UnaryOp:
        match op:
            case syntax.UnaryNegate():
                return tacky.UnaryNegate()
            case syntax.UnaryInvert():
                return tacky.UnaryInvert()
            case _:
                raise Exception(f'unhandled unary operator {op}')
