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
            case _:
                raise Exception(f'unhandled expression type, {expr}')

    def convert_unary_op(self, op: syntax.UnaryOp) -> tacky.UnaryOp:
        match op:
            case syntax.UnaryNegate():
                return tacky.UnaryNegate()
            case syntax.UnaryInvert():
                return tacky.UnaryInvert()
            case _:
                raise Exception(f'unhandled unary operator {op}')
