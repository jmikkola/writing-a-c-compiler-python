from collections import defaultdict
from collections import namedtuple

import syntax
from errors import ValidationError


def validate(program: syntax.Program) -> syntax.Program:
    program = VariableValidator().validate(program)
    program = LabelValidator().validate(program)
    program = LoopLabels().validate(program)
    return program


class MapEntry(namedtuple('MapEntry', ['new_name', 'from_current_block'])):
    ''' for VariableValidator to track where a variable is from '''
    def mark_old(self):
        return MapEntry(self.new_name, False)


def copy_variable_map(variable_map: dict) -> dict:
    return {
        name: entry.mark_old()
        for (name, entry) in variable_map.items()
    }


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

    def validate_function(self, function: syntax.FuncDeclaration):
        variable_map = {}
        body = self.validate_block(function.body, variable_map)
        return syntax.FuncDeclaration(function.name, function.params, body)

    def validate_block(self, block: syntax.Block, variable_map: dict):
        inner_variable_map = copy_variable_map(variable_map)
        block_items = [
            self.validate_statements(b, inner_variable_map)
            for b in block.block_items
        ]
        return syntax.Block(block_items)

    def validate_statements(self, block_item: syntax.BlockItem, variable_map: dict):
        match block_item:
            case syntax.Declaration(name, init):
                if name in variable_map and variable_map[name].from_current_block:
                    self.error(f'{name} already declared')
                variable_map[name] = MapEntry(self.make_unique(name), True)
                if init:
                    init = self.resolve_expr(init, variable_map)
                return syntax.Declaration(variable_map[name].new_name, init)
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
            case syntax.Compound(block):
                block = self.validate_block(block, variable_map)
                return syntax.Compound(block)
            case syntax.Break(_):
                return block_item
            case syntax.Continue(_):
                return block_item
            case syntax.While(test, body, loop_label):
                test = self.resolve_expr(test, variable_map)
                body = self.validate_statements(body, variable_map)
                return syntax.While(test, body, loop_label)
            case syntax.DoWhile(body, test, loop_label):
                body = self.validate_statements(body, variable_map)
                test = self.resolve_expr(test, variable_map)
                return syntax.DoWhile(body, test, loop_label)
            case syntax.For(init, condition, post, body, loop_label):
                inner_variable_map = copy_variable_map(variable_map)
                init = self.resolve_for_init(init, inner_variable_map)
                if condition:
                    condition = self.resolve_expr(condition, inner_variable_map)
                if post:
                    post = self.resolve_expr(post, inner_variable_map)
                body = self.validate_statements(body, inner_variable_map)
                return syntax.For(init, condition, post, body, loop_label)
            case syntax.Switch(condition, body, switch_label, case_values):
                condition = self.resolve_expr(condition, variable_map)
                body = self.validate_statements(body, variable_map)
                return syntax.Switch(condition, body, switch_label, case_values)
            case syntax.Case(value, stmt, switch_label):
                if stmt:
                    stmt = self.validate_statements(stmt, variable_map)
                return syntax.Case(value, stmt, switch_label)
            case syntax.Default(stmt, switch_label):
                if stmt:
                    stmt = self.validate_statements(stmt, variable_map)
                return syntax.Default(stmt, switch_label)
            case _:
                raise Exception(f'unhandled type of block item {block_item}')

    def resolve_for_init(self, init: syntax.ForInit, variable_map: dict):
        match init:
            case syntax.InitDecl(decl):
                decl = self.validate_statements(decl, variable_map)
                return syntax.InitDecl(decl)
            case syntax.InitExp(exp):
                if exp:
                    exp = self.resolve_expr(exp, variable_map)
                return syntax.InitExp(exp)
            case _:
                raise Exception(f'invalid type for ForInit {init}')

    def resolve_expr(self, expr: syntax.Expression, variable_map: dict):
        match expr:
            case syntax.Constant(_):
                return expr
            case syntax.Variable(name):
                if name not in variable_map:
                    self.error(f'undefined variable {name}')
                return syntax.Variable(variable_map[name].new_name)
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

    def validate_function(self, function: syntax.FuncDeclaration):
        labels_declared = set()
        labels_used = set()

        body = self.validate_block(function.body, labels_declared, labels_used)

        labels_not_defined = labels_used - labels_declared
        if labels_not_defined:
            self.error(f'labels are not defined: {labels_not_defined}')

        return syntax.FuncDeclaration(function.name, function.params, body)

    def validate_block(self, block: syntax.Block, labels_declared, labels_used):
        block_items = [
            self.validate_statements(b, labels_declared, labels_used)
            for b in block.block_items
        ]
        return syntax.Block(block_items)

    def validate_statements(self, block_item: syntax.BlockItem, labels_declared: set, labels_used: set):
        match block_item:
            case syntax.Declaration(_, _) | \
                 syntax.Return(_) | \
                 syntax.Continue(_) | \
                 syntax.Break(_) | \
                 syntax.ExprStmt(_) | \
                 syntax.NullStatement():
                return block_item
            case syntax.IfStatement(test, t, e):
                t = self.validate_statements(t, labels_declared, labels_used)
                if e:
                    e = self.validate_statements(e, labels_declared, labels_used)
                return syntax.IfStatement(test, t, e)
            case syntax.While(test, body, loop_label):
                body = self.validate_statements(body, labels_declared, labels_used)
                return syntax.While(test, body, loop_label)
            case syntax.DoWhile(body, test, loop_label):
                body = self.validate_statements(body, labels_declared, labels_used)
                return syntax.DoWhile(body, test, loop_label)
            case syntax.For(init, condition, post, body, loop_label):
                body = self.validate_statements(body, labels_declared, labels_used)
                return syntax.For(init, condition, post, body, loop_label)
            case syntax.Goto(label):
                labels_used.add(label)
                return block_item
            case syntax.LabeledStmt(label, stmt):
                if label in labels_declared:
                    self.error(f'Duplicate definition of the label {label}')
                labels_declared.add(label)
                stmt = self.validate_statements(stmt, labels_declared, labels_used)
                return syntax.LabeledStmt(label, stmt)
            case syntax.Compound(block):
                block = self.validate_block(block, labels_declared, labels_used)
                return syntax.Compound(block)
            case syntax.Switch(condition, body, switch_label, case_values):
                body = self.validate_statements(body, labels_declared, labels_used)
                return syntax.Switch(condition, body, switch_label, case_values)
            case syntax.Case(value, stmt, switch_label):
                if stmt:
                    stmt = self.validate_statements(stmt, labels_declared, labels_used)
                return syntax.Case(value, stmt, switch_label)
            case syntax.Default(stmt, switch_label):
                if stmt:
                    stmt = self.validate_statements(stmt, labels_declared, labels_used)
                return syntax.Default(stmt, switch_label)
            case _:
                raise Exception(f'unhandled type of block item {block_item}')


class LoopScope(namedtuple('LoopScope', ['continue_target', 'break_target', 'switch_label'])):
    ''' For LoopLabels '''
    def for_loop(self, loop_label):
        return LoopScope(loop_label, loop_label, self.switch_label)

    def for_switch(self, switch_label):
        ''' Switch statements change where 'break' goes but not where 'continue' goes '''
        return LoopScope(self.continue_target, switch_label, switch_label)


class LoopLabels:
    ''' This handles both loops and switch statements '''
    def __init__(self):
        self._n_labels = 1
        self._switch_case_values = defaultdict(set)

    def next_label(self):
        label = self._n_labels
        self._n_labels += 1
        return label

    def error(self, msg):
        raise ValidationError(msg)

    def validate(self, program):
        function = self.validate_function(program.function_definition)
        return syntax.Program(function)

    def validate_function(self, function: syntax.FuncDeclaration):
        loop_scope = LoopScope(None, None, None)
        body = self.validate_block(function.body, loop_scope)
        return syntax.FuncDeclaration(function.name, function.params, body)

    def validate_block(self, block: syntax.Block, scope):
        block_items = [
            self.validate_statements(b, scope)
            for b in block.block_items
        ]
        return syntax.Block(block_items)

    def validate_statements(self, block_item: syntax.BlockItem, scope):
        match block_item:
            case syntax.Declaration(_, _) | \
                 syntax.Return(_) | \
                 syntax.ExprStmt(_) | \
                 syntax.Goto(_) | \
                 syntax.NullStatement():
                return block_item
            case syntax.IfStatement(test, t, e):
                t = self.validate_statements(t, scope)
                if e:
                    e = self.validate_statements(e, scope)
                return syntax.IfStatement(test, t, e)
            case syntax.While(test, body, _):
                loop_label = self.next_label()
                loop_scope = scope.for_loop(loop_label)
                body = self.validate_statements(body, loop_scope)
                return syntax.While(test, body, loop_label)
            case syntax.DoWhile(body, test, _):
                loop_label = self.next_label()
                loop_scope = scope.for_loop(loop_label)
                body = self.validate_statements(body, loop_scope)
                return syntax.DoWhile(body, test, loop_label)
            case syntax.For(init, condition, post, body, _):
                loop_label = self.next_label()
                loop_scope = scope.for_loop(loop_label)
                body = self.validate_statements(body, loop_scope)
                return syntax.For(init, condition, post, body, loop_label)
            case syntax.Continue(_):
                if scope.continue_target:
                    return syntax.Continue(scope.continue_target)
                else:
                    self.error('continue statement is not in a loop')
            case syntax.Break(_):
                if scope.break_target:
                    return syntax.Break(scope.break_target)
                else:
                    self.error('break statement is not in a loop or switch statement')
            case syntax.LabeledStmt(label, stmt):
                stmt = self.validate_statements(stmt, scope)
                return syntax.LabeledStmt(label, stmt)
            case syntax.Compound(block):
                block = self.validate_block(block, scope)
                return syntax.Compound(block)
            case syntax.Switch(condition, body, _, _):
                switch_label = self.next_label()
                switch_scope = scope.for_switch(switch_label)
                body = self.validate_statements(body, switch_scope)
                case_values = self._switch_case_values[switch_label]
                return syntax.Switch(condition, body, switch_label, case_values)
            case syntax.Case(value, stmt, _):
                switch_label = scope.switch_label
                if switch_label is None:
                    self.fail('case label outside of a switch statement')
                if not isinstance(value, syntax.Constant):
                    self.fail(f'case value must be a constant, got {value}')
                constant = value.value
                if constant in self._switch_case_values[switch_label]:
                    self.fail(f'duplicate case labels with the value {constant}')
                self._switch_case_values[switch_label].add(constant)
                if stmt:
                    stmt = self.validate_statements(stmt, scope)
                return syntax.Case(value, stmt, switch_label)
            case syntax.Default(stmt, _):
                switch_label = scope.switch_label
                if switch_label is None:
                    self.fail('default outside of a switch statement')
                if 'default' in self._switch_case_values[switch_label]:
                    self.fail('duplicate default labels in a switch statement')
                self._switch_case_values[switch_label].add('default')
                if stmt:
                    stmt = self.validate_statements(stmt, scope)
                return syntax.Default(stmt, switch_label)
            case _:
                raise Exception(f'unhandled type of block item {block_item}')
