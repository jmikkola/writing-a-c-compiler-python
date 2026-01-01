import syntax


def validate(program: syntax.Program) -> syntax.Program:
    v = Validator()
    return v.validate(program)


class Validator:
    def __init__(self):
        self._n_vars = 0

    def make_unique(self, name):
        result = f'{name}.{self._n_vars}'
        self._n_vars += 1
        return result

    def error(self, msg):
        raise ValidationError(msg)

    def validate(self, program):
        function = self.validate_function(program.function_definition)
        return syntax.Program(function)

    def validate_function(self, function: syntax.Function):
        variable_map = {}
        body = [
            self.block_item(b, variable_map)
            for b in function.body
        ]
        return syntax.Function(function.name, body)

    def block_item(self, block_item: syntax.BlockItem, variable_map: dict):
        match block_item:
            case syntax.Declaration(name, init):
                if name in variable_map:
                    self.error(f'{name} already declared')
                variable_map[name] = self.make_unique(name)
                if init:
                    init = self.resolve_expr(init, variable_map)
                return syntax.Declaration(variable_map[name], init)
            case syntax.Return(expr):
                expr = self.resolve_expr(expr, variable_map)
                return syntax.Return(expr)
            case syntax.ExprStmt(expr):
                expr = self.resolve_expr(expr, variable_map)
                return syntax.ExprStmt(expr)
            case syntax.NullStatement():
                return block_item
            case _:
                raise Exception(f'unhandled type of block item {block_item}')

    def resolve_expr(self, expr: syntax.Expression, variable_map: dict):
        match expr:
            case syntax.Constant(_):
                return expr
            case syntax.Variable(name):
                if name not in variable_map:
                    self.error(f'undefined variable {name}')
                return syntax.Variable(variable_map[name])
            case syntax.Unary(op, expr):
                if self.is_modifying_operator(op) and not self.is_variable(expr):
                    self.error(f'invaild to apply {op} to {expr}')
                expr = self.resolve_expr(expr, variable_map)
                return syntax.Unary(op, expr)
            case syntax.Postfix(expr, op):
                if self.is_modifying_operator(op) and not self.is_variable(expr):
                    self.error(f'invaild to apply {op} to {expr}')
                expr = self.resolve_expr(expr, variable_map)
                return syntax.Postfix(expr, op)
            case syntax.Binary(op, left, right):
                left = self.resolve_expr(left, variable_map)
                right = self.resolve_expr(right, variable_map)
                return syntax.Binary(op, left, right)
            case syntax.Assignment(lhs, rhs, op):
                if not isinstance(lhs, syntax.Variable):
                    self.error(f'invalid target for assignment: {lhs}')
                lhs = self.resolve_expr(lhs, variable_map)
                rhs = self.resolve_expr(rhs, variable_map)
                return syntax.Assignment(lhs, rhs, op)
            case _:
                raise Exception(f'unhandled type of expression {expr}')

    def is_modifying_operator(self, op: syntax.UnaryOp):
        match op:
            case syntax.UnaryIncrement() | syntax.UnaryDecrement():
                return True
            case _:
                return False

    def is_variable(self, expr: syntax.Expression):
        match expr:
            case syntax.Variable(_):
                return True
            case _:
                return False


class ValidationError(Exception):
    pass
