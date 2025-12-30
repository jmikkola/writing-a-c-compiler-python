import assembly
import syntax


def gen(syntax: syntax.Program) -> assembly.Program:
    cg = Codegen(syntax)
    return cg.generate()


class Codegen:
    def __init__(self, syntax):
        self.syntax = syntax

    def generate(self):
        function = self.gen_function(self.syntax.function_definition)
        return assembly.Program(function_definition=function)

    def gen_function(self, function: syntax.Function) -> assembly.Function:
        instructions = self.gen_instructions(function.body)
        return assembly.Function(name=function.name, instructions=instructions)

    def gen_instructions(self, body: syntax.Statement) -> list:
        match body:
            case syntax.Return(expr=_):
                return self.gen_return(body)
            case _:
                raise Exception(f'unhandled statement type, {body}')

    def gen_return(self, stmt: syntax.Return) -> list:
        src = self.gen_value(stmt.expr)
        return [
            assembly.Mov(src=src, dst=assembly.Register()),
            assembly.Ret(),
        ]

    def gen_value(self, expr: syntax.Expression) -> assembly.Operand:
        match expr:
            case syntax.Constant(value=_):
                return assembly.Immediate(value=expr.value)
            case _:
                raise Exception(f'unhandled expression type, {expr}')
