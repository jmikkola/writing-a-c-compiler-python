import syntax


def validate(program: syntax.Program) -> syntax.Program:
    program = VariableValidator().validate(program)
    program = LabelValidator().validate(program)
    return program


class VariableValidator:
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
            self.validate_statements(b, variable_map)
            for b in function.body
        ]
        return syntax.Function(function.name, body)

    def validate_statements(self, block_item: syntax.BlockItem, variable_map: dict):
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
            case syntax.IfStatement(test, t, e):
                test = self.resolve_expr(test, variable_map)
                t = self.validate_statements(t, variable_map)
                if e:
                    e = self.validate_statements(e, variable_map)
                return syntax.IfStatement(test, t, e)
            case syntax.Goto(_):
                return block_item
            case syntax.LabeledStmt(label, stmt):
                stmt = self.validate_statements(stmt, variable_map)
                return syntax.LabeledStmt(label, stmt)
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
            case syntax.Conditional(test, t, e):
                test = self.resolve_expr(test, variable_map)
                t = self.resolve_expr(t, variable_map)
                e = self.resolve_expr(e, variable_map)
                return syntax.Conditional(test, t, e)
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

class LabelValidator:
    def error(self, msg):
        raise ValidationError(msg)

    def validate(self, program):
        function = self.validate_function(program.function_definition)
        return syntax.Program(function)

    def validate_function(self, function: syntax.Function):
        labels_declared = set()
        labels_used = set()
        body = [
            self.validate_statements(b, labels_declared, labels_used)
            for b in function.body
        ]
        labels_not_defined = labels_used - labels_declared
        if labels_not_defined:
            self.error(f'labels are not defined: {labels_not_defined}')
        return syntax.Function(function.name, body)

    def validate_statements(self, block_item: syntax.BlockItem, labels_declared: set, labels_used: set):
        match block_item:
            case syntax.Declaration(_, _) | \
                 syntax.Return(_) | \
                 syntax.ExprStmt(_) | \
                 syntax.NullStatement():
                return block_item
            case syntax.IfStatement(test, t, e):
                t = self.validate_statements(t, labels_declared, labels_used)
                if e:
                    e = self.validate_statements(e, labels_declared, labels_used)
                return syntax.IfStatement(test, t, e)
            case syntax.Goto(label):
                labels_used.add(label)
                return block_item
            case syntax.LabeledStmt(label, stmt):
                if label in labels_declared:
                    self.error(f'Duplicate definition of the label {label}')
                labels_declared.add(label)
                stmt = self.validate_statements(stmt, labels_declared, labels_used)
                return syntax.LabeledStmt(label, stmt)
            case _:
                raise Exception(f'unhandled type of block item {block_item}')


class ValidationError(Exception):
    pass
