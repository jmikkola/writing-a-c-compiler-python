from collections import defaultdict
from collections import namedtuple

import syntax
from errors import ValidationError, TypeError


def validate(program: syntax.Program) -> syntax.Program:
    program = IdentifierResolution().validate(program)
    program = LabelValidator().validate(program)
    program = LoopLabels().validate(program)
    Typecheck().typecheck(program)
    return program


class MapEntry(namedtuple('MapEntry', ['new_name', 'from_current_scope', 'has_linkage'])):
    ''' for IdentifierResolution to track where a variable is from '''
    def mark_old(self):
        return MapEntry(self.new_name, False, self.has_linkage)

    @classmethod
    def for_name(self, name, has_linkage=False):
        return MapEntry(name, True, has_linkage)


def copy_identifier_map(identifier_map: dict) -> dict:
    return {
        name: entry.mark_old()
        for (name, entry) in identifier_map.items()
    }


class IdentifierResolution:
    def __init__(self):
        self._n_vars = 0

    def make_unique(self, name):
        result = f'{name}.{self._n_vars}'
        self._n_vars += 1
        return result

    def error(self, msg):
        raise ValidationError(msg)

    def validate(self, program):
        identifier_map = {}
        functions = [
            self.validate_function(f, identifier_map)
            for f in program.declarations
        ]
        return syntax.Program(functions)

    def validate_function(self, function: syntax.FuncDeclaration, identifier_map):
        name = function.name
        if name in identifier_map:
            prev_entry = identifier_map[name]
            if prev_entry.from_current_scope and not prev_entry.has_linkage:
                self.error(f'duplicate declaration of {name}')

        identifier_map[name] = MapEntry.for_name(name, has_linkage=True)

        inner_identifier_map = copy_identifier_map(identifier_map)

        new_params = []
        for param in function.params:
            new_params.append(self.resolve_param(param, inner_identifier_map))

        body = function.body
        if body:
            block_items = [
                self.validate_statements(b, inner_identifier_map)
                for b in body.block_items
            ]
            body = syntax.Block(block_items)
        return syntax.FuncDeclaration(function.name, new_params, body, function.storage_class)

    def resolve_param(self, param, identifier_map):
        if param in identifier_map and identifier_map[param].from_current_scope:
            self.error(f'{param} already declared')
        new_name = self.make_unique(param)
        identifier_map[param] = MapEntry.for_name(new_name)
        return new_name

    def validate_block(self, block: syntax.Block, identifier_map: dict):
        inner_identifier_map = copy_identifier_map(identifier_map)
        block_items = [
            self.validate_statements(b, inner_identifier_map)
            for b in block.block_items
        ]
        return syntax.Block(block_items)

    def validate_statements(self, block_item: syntax.BlockItem, identifier_map: dict):
        match block_item:
            case syntax.VarDeclaration(name, init, storage_class):
                if name in identifier_map and identifier_map[name].from_current_scope:
                    self.error(f'{name} already declared')
                identifier_map[name] = MapEntry.for_name(self.make_unique(name))
                if init:
                    init = self.resolve_expr(init, identifier_map)
                return syntax.VarDeclaration(
                    identifier_map[name].new_name,
                    init,
                    storage_class
                )
            case syntax.FuncDeclaration(name, _, body, _):
                if body is not None:
                    self.error('function definitions not allowed inside another function')
                return self.validate_function(block_item, identifier_map)
            case syntax.Return(expr):
                expr = self.resolve_expr(expr, identifier_map)
                return syntax.Return(expr)
            case syntax.ExprStmt(expr):
                expr = self.resolve_expr(expr, identifier_map)
                return syntax.ExprStmt(expr)
            case syntax.NullStatement():
                return block_item
            case syntax.IfStatement(test, t, e):
                test = self.resolve_expr(test, identifier_map)
                t = self.validate_statements(t, identifier_map)
                if e:
                    e = self.validate_statements(e, identifier_map)
                return syntax.IfStatement(test, t, e)
            case syntax.Goto(_):
                return block_item
            case syntax.LabeledStmt(label, stmt):
                stmt = self.validate_statements(stmt, identifier_map)
                return syntax.LabeledStmt(label, stmt)
            case syntax.Compound(block):
                block = self.validate_block(block, identifier_map)
                return syntax.Compound(block)
            case syntax.Break(_):
                return block_item
            case syntax.Continue(_):
                return block_item
            case syntax.While(test, body, loop_label):
                test = self.resolve_expr(test, identifier_map)
                body = self.validate_statements(body, identifier_map)
                return syntax.While(test, body, loop_label)
            case syntax.DoWhile(body, test, loop_label):
                body = self.validate_statements(body, identifier_map)
                test = self.resolve_expr(test, identifier_map)
                return syntax.DoWhile(body, test, loop_label)
            case syntax.For(init, condition, post, body, loop_label):
                inner_identifier_map = copy_identifier_map(identifier_map)
                init = self.resolve_for_init(init, inner_identifier_map)
                if condition:
                    condition = self.resolve_expr(condition, inner_identifier_map)
                if post:
                    post = self.resolve_expr(post, inner_identifier_map)
                body = self.validate_statements(body, inner_identifier_map)
                return syntax.For(init, condition, post, body, loop_label)
            case syntax.Switch(condition, body, switch_label, case_values):
                condition = self.resolve_expr(condition, identifier_map)
                body = self.validate_statements(body, identifier_map)
                return syntax.Switch(condition, body, switch_label, case_values)
            case syntax.Case(value, stmt, switch_label):
                if stmt:
                    stmt = self.validate_statements(stmt, identifier_map)
                return syntax.Case(value, stmt, switch_label)
            case syntax.Default(stmt, switch_label):
                if stmt:
                    stmt = self.validate_statements(stmt, identifier_map)
                return syntax.Default(stmt, switch_label)
            case _:
                raise Exception(f'unhandled type of block item {block_item}')

    def resolve_for_init(self, init: syntax.ForInit, identifier_map: dict):
        match init:
            case syntax.InitDecl(decl):
                decl = self.validate_statements(decl, identifier_map)
                return syntax.InitDecl(decl)
            case syntax.InitExp(exp):
                if exp:
                    exp = self.resolve_expr(exp, identifier_map)
                return syntax.InitExp(exp)
            case _:
                raise Exception(f'invalid type for ForInit {init}')

    def resolve_expr(self, expr: syntax.Expression, identifier_map: dict):
        match expr:
            case syntax.Constant(_):
                return expr
            case syntax.Variable(name):
                if name not in identifier_map:
                    self.error(f'undefined variable {name}')
                return syntax.Variable(identifier_map[name].new_name)
            case syntax.Unary(op, expr):
                if self.is_modifying_operator(op) and not self.is_variable(expr):
                    self.error(f'invaild to apply {op} to {expr}')
                expr = self.resolve_expr(expr, identifier_map)
                return syntax.Unary(op, expr)
            case syntax.Postfix(expr, op):
                if self.is_modifying_operator(op) and not self.is_variable(expr):
                    self.error(f'invaild to apply {op} to {expr}')
                expr = self.resolve_expr(expr, identifier_map)
                return syntax.Postfix(expr, op)
            case syntax.Binary(op, left, right):
                left = self.resolve_expr(left, identifier_map)
                right = self.resolve_expr(right, identifier_map)
                return syntax.Binary(op, left, right)
            case syntax.Assignment(lhs, rhs, op):
                if not isinstance(lhs, syntax.Variable):
                    self.error(f'invalid target for assignment: {lhs}')
                lhs = self.resolve_expr(lhs, identifier_map)
                rhs = self.resolve_expr(rhs, identifier_map)
                return syntax.Assignment(lhs, rhs, op)
            case syntax.Conditional(test, t, e):
                test = self.resolve_expr(test, identifier_map)
                t = self.resolve_expr(t, identifier_map)
                e = self.resolve_expr(e, identifier_map)
                return syntax.Conditional(test, t, e)
            case syntax.Call(func_name, arguments):
                if func_name not in identifier_map:
                    self.error(f'calling an undefined function {func_name}')
                new_func_name = identifier_map[func_name].new_name
                new_args = [self.resolve_expr(arg, identifier_map) for arg in arguments]
                return syntax.Call(new_func_name, new_args)
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
        functions = [
            self.validate_function(f)
            for f in program.declarations
        ]
        return syntax.Program(functions)

    def validate_function(self, function: syntax.FuncDeclaration):
        labels_declared = set()
        labels_used = set()

        body = function.body
        if body:
            body = self.validate_block(body, labels_declared, labels_used)

        labels_not_defined = labels_used - labels_declared
        if labels_not_defined:
            self.error(f'labels are not defined: {labels_not_defined}')

        return syntax.FuncDeclaration(function.name, function.params, body, function.storage_class)

    def validate_block(self, block: syntax.Block, labels_declared, labels_used):
        block_items = [
            self.validate_statements(b, labels_declared, labels_used)
            for b in block.block_items
        ]
        return syntax.Block(block_items)

    def validate_statements(self, block_item: syntax.BlockItem, labels_declared: set, labels_used: set):
        match block_item:
            case syntax.VarDeclaration(_, _, _) | \
                 syntax.FuncDeclaration(_, _, _, _) | \
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
        functions = [
            self.validate_function(f)
            for f in program.declarations
        ]
        return syntax.Program(functions)

    def validate_function(self, function: syntax.FuncDeclaration):
        loop_scope = LoopScope(None, None, None)
        body = function.body
        if body:
            body = self.validate_block(body, loop_scope)
        return syntax.FuncDeclaration(function.name, function.params, body, function.storage_class)

    def validate_block(self, block: syntax.Block, scope):
        block_items = [
            self.validate_statements(b, scope)
            for b in block.block_items
        ]
        return syntax.Block(block_items)

    def validate_statements(self, block_item: syntax.BlockItem, scope):
        match block_item:
            case syntax.VarDeclaration(_, _, _) | \
                 syntax.FuncDeclaration(_, _, _, _) | \
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


class Symbol(namedtuple('Symbol', ['type', 'defined'])):
    pass


class Typecheck:
    def __init__(self):
        self.symbols = {}

    def error(self, msg):
        raise TypeError(msg)

    def typecheck(self, program: syntax.Program):
        for f in program.declarations:
            self.typecheck_func_decl(f)
        return self.symbols

    def typecheck_func_decl(self, f: syntax.FuncDeclaration):
        func_type = syntax.Func(n_params=len(f.params))
        has_body = f.body is not None
        already_defined = False

        if f.name in self.symbols:
            existing = self.symbols.get(f.name)
            if existing.type != func_type:
                self.error(f'mismatched types for {f.name}')
            already_defined = existing.defined
            if already_defined and has_body:
                self.error(f'function {f.name} is defined more than once')

        defined = has_body or already_defined
        self.symbols[f.name] = Symbol(func_type, defined)

        if has_body:
            for param in f.params:
                self.symbols[param] = Symbol(syntax.Int(), True)
            self.typecheck_block(f.body)

    def typecheck_var_decl(self, v: syntax.VarDeclaration):
        self.symbols[v.name] = Symbol(syntax.Int(), True)
        if v.init is not None:
            self.typecheck_expr(v.init)

    def typecheck_block(self, block: syntax.Block):
        for block_item in block.block_items:
            self.typecheck_statement(block_item)

    def typecheck_statement(self, block_item: syntax.BlockItem):
        match block_item:
            case syntax.VarDeclaration(_, _, _):
                self.typecheck_var_decl(block_item)
            case syntax.FuncDeclaration(_, _, _, _):
                self.typecheck_func_decl(block_item)
            case syntax.Return(expr):
                self.typecheck_expr(expr)
            case syntax.ExprStmt(expr):
                self.typecheck_expr(expr)
            case syntax.Goto(_) | \
                 syntax.NullStatement():
                pass
            case syntax.IfStatement(test, t, e):
                self.typecheck_expr(test)
                self.typecheck_statement(t)
                if e:
                    self.typecheck_statement(e)
            case syntax.While(test, body, _):
                self.typecheck_expr(test)
                self.typecheck_statement(body)
            case syntax.DoWhile(body, test, _):
                self.typecheck_expr(test)
                self.typecheck_statement(body)
            case syntax.For(init, condition, post, body, _):
                self.typecheck_for_init(init)
                if condition:
                    self.typecheck_expr(condition)
                if post:
                    self.typecheck_expr(post)
                self.typecheck_statement(body)
            case syntax.Continue(_):
                pass
            case syntax.Break(_):
                pass
            case syntax.LabeledStmt(label, stmt):
                self.typecheck_statement(stmt)
            case syntax.Compound(block):
                self.typecheck_block(block)
            case syntax.Switch(condition, body, _, _):
                self.typecheck_expr(condition)
                self.typecheck_statement(body)
            case syntax.Case(value, stmt, _):
                if stmt:
                    self.typecheck_statement(stmt)
            case syntax.Default(stmt, _):
                if stmt:
                    self.typecheck_statement(stmt)
            case _:
                raise Exception(f'unhandled type of block item {block_item}')

    def typecheck_expr(self, expr: syntax.Expression):
        match expr:
            case syntax.Constant(_):
                pass
            case syntax.Variable(name):
                if isinstance(self.symbols[name].type, syntax.Func):
                    self.error(f'Function {name} used as a variable')
            case syntax.Call(name, arguments):
                symbol = self.symbols[name]
                if not isinstance(symbol.type, syntax.Func):
                    self.error(f'Variable {name} used as a function')
                if symbol.type.n_params != len(arguments):
                    expected = symbol.type.n_params
                    actual = len(arguments)
                    self.error(f'Function {name} expects {expected} arguments, got {actual} arguments')
                for a in arguments:
                    self.typecheck_expr(a)
            case syntax.Unary(_, expr):
                self.typecheck_expr(expr)
            case syntax.Postfix(expr, _):
                self.typecheck_expr(expr)
            case syntax.Binary(_, l, r):
                self.typecheck_expr(l)
                self.typecheck_expr(r)
            case syntax.Assignment(lhs, rhs, _):
                name = lhs.name
                if isinstance(self.symbols[name].type, syntax.Func):
                    self.error(f'Cannot assign to a function {name}')
                self.typecheck_expr(rhs)
            case syntax.Conditional(condition, t, e):
                self.typecheck_expr(condition)
                self.typecheck_expr(t)
                self.typecheck_expr(e)

    def typecheck_for_init(self, init: syntax.ForInit):
        match init:
            case syntax.InitDecl(decl):
                self.typecheck_statement(decl)
            case syntax.InitExp(exp):
                if exp:
                    self.typecheck_expr(exp)
            case _:
                raise Exception(f'invalid type for ForInit {init}')
