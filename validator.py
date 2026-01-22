import typing
from collections import defaultdict
from collections import namedtuple

import syntax
import symbol
from errors import ValidationError, TypeError
import typeconversion


def validate(program: syntax.Program) -> typing.Tuple[syntax.Program, dict]:
    program = IdentifierResolution().validate(program)
    program = LabelValidator().validate(program)
    program = LoopLabels().validate(program)
    typecheck = Typecheck()
    program, symbols = typecheck.typecheck(program)
    return (program, symbols)


class MapEntry(namedtuple('MapEntry', ['new_name', 'from_current_scope', 'has_linkage'])):
    ''' for IdentifierResolution to track where a variable is from '''
    def mark_old(self):
        return MapEntry(self.new_name, False, self.has_linkage)

    @classmethod
    def for_name(cls, name, has_linkage=False):
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
        declarations = [
            self.validate_declaration(d, identifier_map)
            for d in program.declarations
        ]
        return syntax.Program(declarations)

    def validate_declaration(self, decl: syntax.Declaration, identifier_map):
        identifier_map[decl.name] = MapEntry.for_name(decl.name, has_linkage=True)

        match decl:
            case syntax.FuncDeclaration():
                return self.validate_function(decl, identifier_map)
            case syntax.VarDeclaration():
                return decl
            case _:
                raise Exception(f'unhandled kind of declaration {decl}')

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
        return syntax.FuncDeclaration(
            function.name,
            new_params,
            body,
            function.fun_type,
            function.storage_class
        )

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
            case syntax.VarDeclaration():
                return self.validate_block_scope_variable(block_item, identifier_map)
            case syntax.FuncDeclaration(name, _, body, storage_class):
                if body is not None:
                    self.error('function definitions not allowed inside another function')
                if storage_class == syntax.Static():
                    self.error('function declarations inside a block cannot be static')
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

    def validate_block_scope_variable(self, var: syntax.VarDeclaration, identifier_map):
        name = var.name
        is_extern = var.storage_class == syntax.Extern()

        if name in identifier_map:
            prev_entry = identifier_map[name]
            if prev_entry.from_current_scope:
                if not (prev_entry.has_linkage and is_extern):
                    self.error(f'conflicting declarations for {name}')

        if is_extern:
            identifier_map[name] = MapEntry.for_name(name, has_linkage=True)
            return var
        else:
            unique_name = self.make_unique(name)
            identifier_map[name] = MapEntry.for_name(unique_name)

            init = var.init
            if init:
                init = self.resolve_expr(init, identifier_map)

            return syntax.VarDeclaration(unique_name, init, var.var_type, var.storage_class)

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
            case syntax.Cast(target_type, expr):
                expr = self.resolve_expr(expr, identifier_map)
                return syntax.Cast(target_type, expr)
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
        declarations = [
            self.validate_declaration(d)
            for d in program.declarations
        ]
        return syntax.Program(declarations)

    def validate_declaration(self, decl: syntax.Declaration):
        match decl:
            case syntax.FuncDeclaration():
                return self.validate_function(decl)
            case syntax.VarDeclaration():
                return decl
            case _:
                raise Exception(f'unhandled kind of declaration {decl}')

    def validate_function(self, function: syntax.FuncDeclaration):
        labels_declared = set()
        labels_used = set()

        body = function.body
        if body:
            body = self.validate_block(body, labels_declared, labels_used)

        labels_not_defined = labels_used - labels_declared
        if labels_not_defined:
            self.error(f'labels are not defined: {labels_not_defined}')

        return syntax.FuncDeclaration(
            function.name,
            function.params,
            body,
            function.fun_type,
            function.storage_class
        )

    def validate_block(self, block: syntax.Block, labels_declared, labels_used):
        block_items = [
            self.validate_statements(b, labels_declared, labels_used)
            for b in block.block_items
        ]
        return syntax.Block(block_items)

    def validate_statements(self, block_item: syntax.BlockItem, labels_declared: set, labels_used: set):
        match block_item:
            case syntax.VarDeclaration() | \
                 syntax.FuncDeclaration() | \
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
        declarations = [
            self.validate_declaration(d)
            for d in program.declarations
        ]
        return syntax.Program(declarations)

    def validate_declaration(self, decl: syntax.Declaration):
        match decl:
            case syntax.FuncDeclaration():
                return self.validate_function(decl)
            case syntax.VarDeclaration():
                return decl
            case _:
                raise Exception(f'unhandled kind of declaration {decl}')

    def validate_function(self, function: syntax.FuncDeclaration):
        loop_scope = LoopScope(None, None, None)
        body = function.body
        if body:
            body = self.validate_block(body, loop_scope)
        return syntax.FuncDeclaration(
            function.name,
            function.params,
            body,
            function.fun_type,
            function.storage_class
        )

    def validate_block(self, block: syntax.Block, scope):
        block_items = [
            self.validate_statements(b, scope)
            for b in block.block_items
        ]
        return syntax.Block(block_items)

    def validate_statements(self, block_item: syntax.BlockItem, scope):
        match block_item:
            case syntax.VarDeclaration() | \
                 syntax.FuncDeclaration() | \
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
                    self.error('case label outside of a switch statement')
                if not isinstance(value, syntax.Constant):
                    self.error(f'case value must be a constant, got {value}')
                constant = value.const
                if constant in self._switch_case_values[switch_label]:
                    self.error(f'duplicate case labels with the value {constant}')
                self._switch_case_values[switch_label].add(constant)
                if stmt:
                    stmt = self.validate_statements(stmt, scope)
                return syntax.Case(value, stmt, switch_label)
            case syntax.Default(stmt, _):
                switch_label = scope.switch_label
                if switch_label is None:
                    self.error('default outside of a switch statement')
                if 'default' in self._switch_case_values[switch_label]:
                    self.error('duplicate default labels in a switch statement')
                self._switch_case_values[switch_label].add('default')
                if stmt:
                    stmt = self.validate_statements(stmt, scope)
                return syntax.Default(stmt, switch_label)
            case _:
                raise Exception(f'unhandled type of block item {block_item}')


class Typecheck:
    def __init__(self):
        self.symbols = {}
        self.current_return_type = None
        self._switch_condition_types = {}

    def error(self, msg):
        raise TypeError(msg)

    def typecheck(self, program: syntax.Program):
        '''This returns a symbol table.

        It also modifies the expression objects, adding types and casts.
        '''
        declarations = [
            self.typecheck_decl(decl)
            for decl in program.declarations
        ]
        program = syntax.Program(declarations)

        return program, self.symbols

    def typecheck_decl(self, decl: syntax.Declaration):
        match decl:
            case syntax.FuncDeclaration():
                return self.typecheck_func_decl(decl)
            case syntax.VarDeclaration():
                return self.typecheck_var_decl_file_scope(decl)
            case _:
                raise Exception(f'unhandled kind of declaration {decl}')

    def typecheck_func_decl(self, f: syntax.FuncDeclaration, in_block=False):
        func_type = f.fun_type
        self.current_return_type = func_type.ret
        has_body = f.body is not None
        already_defined = False
        is_global = f.storage_class != syntax.Static()

        if in_block and f.storage_class == syntax.Static():
            self.error(f'static not allowed on function declaration inside a function: {f}')

        if f.name in self.symbols:
            existing = self.symbols.get(f.name)
            if existing.type != func_type:
                self.error(f'mismatched type in declarations for {f.name}')
            already_defined = existing.attrs.is_defined
            if already_defined and has_body:
                self.error(f'function {f.name} is defined more than once')

            if existing.attrs.is_global and f.storage_class == syntax.Static():
                self.error(f'Static function declaration {f.name} follows non-static')
            is_global = existing.attrs.is_global

        is_defined = has_body or already_defined
        attrs = symbol.FuncAttr(is_defined, is_global)
        self.symbols[f.name] = symbol.Symbol(func_type, attrs)

        body = f.body
        if has_body:
            for (param_name, param_type) in zip(f.params, func_type.params):
                self.symbols[param_name] = symbol.Symbol(param_type, symbol.LocalAttr())
            body = self.typecheck_block(body)

        return syntax.FuncDeclaration(
            f.name,
            f.params,
            body,
            f.fun_type,
            f.storage_class
        )

    def typecheck_var_decl_file_scope(self, v: syntax.VarDeclaration):
        initial_value = None
        match v.init:
            case syntax.Constant(_):
                initial_value = self.make_static_init(v.name, v.init, v.var_type)
            case None:
                if v.storage_class == syntax.Extern():
                    initial_value = symbol.NoInitializer()
                else:
                    initial_value = symbol.Tentative()
            case _:
                self.error(f'non-constant initializer for {v.name}')

        is_global = v.storage_class != syntax.Static()

        if v.name in self.symbols:
            existing = self.symbols[v.name]
            if isinstance(existing.type, syntax.Func):
                self.error(f'function {v.name} redeclared as a variable')
            if existing.type != v.var_type:
                self.error(f'variable {v.name} redeclared with a different type')
            if v.storage_class == syntax.Extern():
                is_global = existing.attrs.is_global
            elif existing.attrs.is_global != is_global:
                self.error(f'conflicting linkage for {v.name}')

            if isinstance(existing.attrs.init, symbol.Initial):
                if isinstance(initial_value, symbol.Initial):
                    self.error(f'conflicting file scope variable definitions for {v.name}')
                else:
                    initial_value = existing.attrs.init
            elif not isinstance(initial_value, symbol.Initial) and isinstance(existing.attrs.init, symbol.Tentative):
                initial_value = symbol.Tentative()

        attrs = symbol.StaticAttr(init=initial_value, is_global=is_global)
        self.symbols[v.name] = symbol.Symbol(v.var_type, attrs)
        return v

    def typecheck_var_decl_block_scope(self, v: syntax.VarDeclaration):
        if v.storage_class == syntax.Extern():
            if v.init is not None:
                self.error(f'initializer on local extern variable declaration for {v.name}')
            if v.name in self.symbols:
                existing = self.symbols[v.name]
                if isinstance(existing.type, syntax.Func):
                    self.error(f'function {v.name} redeclared as a variable')
                if existing.type != v.var_type:
                    self.error(f'variable {v.name} redeclared with a different type')
            else:
                attrs = symbol.StaticAttr(symbol.NoInitializer(), True)
                self.symbols[v.name] = symbol.Symbol(v.var_type, attrs)

        elif v.storage_class == syntax.Static():
            initial_value = self.make_static_init(v.name, v.init, v.var_type)
            attrs = symbol.StaticAttr(initial_value, False)
            self.symbols[v.name] = symbol.Symbol(v.var_type, attrs)

        else:
            self.symbols[v.name] = symbol.Symbol(v.var_type, symbol.LocalAttr())
            if v.init is not None:
                init = self.typecheck_expr(v.init)
                init = self.convert_to(init, v.var_type)
                return syntax.VarDeclaration(v.name, init, v.var_type, v.storage_class)
        return v

    def make_static_init(self, name, init, var_type):
        match init:
            case None:
                init_value = self.to_static_init(0, var_type)
                return symbol.Initial(init_value)
            case syntax.Constant(syntax.ConstLong(value)):
                init_value = self.to_static_init(value, var_type)
                return symbol.Initial(init_value)
            case syntax.Constant(syntax.ConstInt(value)):
                init_value = self.to_static_init(value, var_type)
                return symbol.Initial(init_value)
            case syntax.Constant(syntax.ConstULong(value)):
                init_value = self.to_static_init(value, var_type)
                return symbol.Initial(init_value)
            case syntax.Constant(syntax.ConstUInt(value)):
                init_value = self.to_static_init(value, var_type)
                return symbol.Initial(init_value)
            case syntax.Constant(syntax.ConstDouble(value)):
                init_value = self.to_static_init(value, var_type)
                return symbol.Initial(init_value)
            case syntax.Constant(x):
                raise Exception(f'unhandled type of constant {x}')
            case _:
                self.error(f'non-constant initializer for {name}')

    def to_static_init(self, value, var_type):
        match var_type:
            case syntax.Long():
                value = typeconversion.constant_to_long(value)
                return symbol.LongInit(value)
            case syntax.Int():
                value = typeconversion.constant_to_int(value)
                return symbol.IntInit(value)
            case syntax.ULong():
                value = typeconversion.constant_to_long(value, unsigned=True)
                return symbol.ULongInit(value)
            case syntax.UInt():
                value = typeconversion.constant_to_int(value, unsigned=True)
                return symbol.UIntInit(value)
            case syntax.Double():
                if isinstance(value, int):
                    value = float(value)
                return symbol.DoubleInit(value)
            case _:
                raise Exception(f'unhandled type for static constant {var_type}')

    def typecheck_block(self, block: syntax.Block):
        block_items = [
            self.typecheck_statement(block_item)
            for block_item in block.block_items
        ]
        return syntax.Block(block_items)

    def typecheck_statement(self, block_item: syntax.BlockItem):
        match block_item:
            case syntax.VarDeclaration():
                return self.typecheck_var_decl_block_scope(block_item)
            case syntax.FuncDeclaration():
                return self.typecheck_func_decl(block_item, in_block=True)
            case syntax.Return(expr):
                expr = self.typecheck_expr(expr)
                expr = self.convert_to(expr, self.current_return_type)
                return syntax.Return(expr)
            case syntax.ExprStmt(expr):
                expr = self.typecheck_expr(expr)
                return syntax.ExprStmt(expr)
            case syntax.Goto(_) | \
                 syntax.NullStatement():
                return block_item
            case syntax.IfStatement(test, t, e):
                test = self.typecheck_expr(test)
                t = self.typecheck_statement(t)
                if e:
                    e = self.typecheck_statement(e)
                return syntax.IfStatement(test, t, e)
            case syntax.While(test, body, loop_label):
                test = self.typecheck_expr(test)
                body = self.typecheck_statement(body)
                return syntax.While(test, body, loop_label)
            case syntax.DoWhile(body, test, loop_label):
                test = self.typecheck_expr(test)
                body = self.typecheck_statement(body)
                return syntax.DoWhile(body, test, loop_label)
            case syntax.For(init, condition, post, body, loop_label):
                init = self.typecheck_for_init(init)
                if condition:
                    condition = self.typecheck_expr(condition)
                if post:
                    post = self.typecheck_expr(post)
                body = self.typecheck_statement(body)
                return syntax.For(init, condition, post, body, loop_label)
            case syntax.Continue(_):
                return block_item
            case syntax.Break(_):
                return block_item
            case syntax.LabeledStmt(label, stmt):
                stmt = self.typecheck_statement(stmt)
                return syntax.LabeledStmt(label, stmt)
            case syntax.Compound(block):
                block = self.typecheck_block(block)
                return syntax.Compound(block)
            case syntax.Switch(condition, body, switch_label, case_values):
                condition = self.typecheck_expr(condition)
                condition_type = condition.expr_type
                self._switch_condition_types[switch_label] = condition_type
                body = self.typecheck_statement(body)
                case_values = self.convert_case_values(case_values, condition_type)
                return syntax.Switch(condition, body, switch_label, case_values)
            case syntax.Case(value, stmt, switch_label):
                condition_type = self._switch_condition_types[switch_label]
                value = syntax.Constant(self.convert_case_value(value.const, condition_type))
                if stmt:
                    stmt = self.typecheck_statement(stmt)
                return syntax.Case(value, stmt, switch_label)
            case syntax.Default(stmt, switch_label):
                if stmt:
                    stmt = self.typecheck_statement(stmt)
                return syntax.Default(stmt, switch_label)
            case _:
                raise Exception(f'unhandled type of block item {block_item}')

    def convert_case_values(self, case_values: set, target_type: syntax.Type) -> set:
        updated_values = set()
        for value in case_values:
            if value == 'default':
                converted = value
            else:
                converted = self.convert_case_value(value, target_type)
            if converted in updated_values:
                self.error(f'multiple case values are equivalent to {converted}')
            else:
                updated_values.add(converted)
        return updated_values

    def convert_case_value(self, value, target_type: syntax.Type):
        match value:
            case syntax.ConstInt(n):
                return self.convert_integer(n, target_type)
            case syntax.ConstLong(n):
                return self.convert_integer(n, target_type)
            case syntax.ConstUInt(n):
                return self.convert_integer(n, target_type)
            case syntax.ConstULong(n):
                return self.convert_integer(n, target_type)
            case _:
                raise Exception(f'unexpected constant value in convert_case_value: {value}')

    def convert_integer(self, value, target_type: syntax.Type):
        match target_type:
            case syntax.Long():
                value = typeconversion.constant_to_long(value)
                return syntax.ConstLong(value)
            case syntax.Int():
                value = typeconversion.constant_to_int(value)
                return syntax.ConstInt(value)
            case syntax.ULong():
                value = typeconversion.constant_to_long(value, unsigned=True)
                return syntax.ConstULong(value)
            case syntax.UInt():
                value = typeconversion.constant_to_int(value, unsigned=True)
                return syntax.ConstUInt(value)
            case _:
                raise Exception(f'unhandled type for constant {target_type}')

    def typecheck_expr(self, expr: syntax.Expression):
        match expr:
            case syntax.Constant(const):
                match const:
                    case syntax.ConstInt(_):
                        return expr.set_type(syntax.Int())
                    case syntax.ConstLong(_):
                        return expr.set_type(syntax.Long())
                    case syntax.ConstUInt(_):
                        return expr.set_type(syntax.UInt())
                    case syntax.ConstULong(_):
                        return expr.set_type(syntax.ULong())
                    case syntax.ConstDouble(_):
                        return expr.set_type(syntax.Double())
                    case _:
                        raise Exception(f'unhandled type of const {const}')

            case syntax.Variable(name):
                symbol_type = self.symbols[name].type
                if isinstance(symbol_type, syntax.Func):
                    self.error(f'Function {name} used as a variable')
                return expr.set_type(symbol_type)

            case syntax.Call(name, arguments):
                symbol_type = self.symbols[name].type

                if not isinstance(symbol_type, syntax.Func):
                    self.error(f'Variable {name} used as a function')
                if len(symbol_type.params) != len(arguments):
                    expected = len(symbol_type.params)
                    actual = len(arguments)
                    self.error(f'Function {name} expects {expected} arguments, got {actual} arguments')

                converted_arguments = []
                for (arg, param_type) in zip(arguments, symbol_type.params):
                    arg = self.typecheck_expr(arg)
                    converted_arg = self.convert_to(arg, param_type)
                    converted_arguments.append(converted_arg)

                expr = syntax.Call(name, converted_arguments)
                return expr.set_type(symbol_type.ret)

            case syntax.Unary(op, e):
                e = self.typecheck_expr(e)
                if e.expr_type == syntax.Double():
                    if op == syntax.UnaryInvert():
                        self.error(f'Cannot apply unary op {op} to a double')
                expr = syntax.Unary(op, e)
                if isinstance(op, syntax.UnaryNot()):
                    return expr.set_type(syntax.Int())
                else:
                    return expr.set_type(e.expr_type)

            case syntax.Postfix(e, op):
                e = self.typecheck_expr(e)
                expr = syntax.Postfix(e, op)
                return expr.set_type(e.expr_type)

            case syntax.Binary(op, l, r):
                l = self.typecheck_expr(l)
                r = self.typecheck_expr(r)

                common_type = self.get_common_type(l.expr_type, r.expr_type)

                if common_type == syntax.Double():
                    int_only_ops = [
                        syntax.BinaryRemainder(),
                        syntax.BitAnd(),
                        syntax.BitOr(),
                        syntax.BitXor(),
                        syntax.ShiftLeft(),
                        syntax.ShiftRight(),
                    ]
                    if op in int_only_ops:
                        self.error(f'Cannot apply operator {op} to doubles')

                # && and || always return an int
                if op == syntax.BinaryAnd() or op == syntax.BinaryOr():
                    expr = syntax.Binary(op, l, r)
                    return expr.set_type(syntax.Int())

                # << and >> always take the type of the left hand side
                if op == syntax.ShiftLeft() or op == syntax.ShiftRight():
                    expr = syntax.Binary(op, l, r)
                    return expr.set_type(l.expr_type)

                converted_l = self.convert_to(l, common_type)
                converted_r = self.convert_to(r, common_type)
                expr = syntax.Binary(op, converted_l, converted_r)

                comparison_operators = [
                    syntax.Less(),
                    syntax.LessEqual(),
                    syntax.Greater(),
                    syntax.GreaterEqual(),
                    syntax.Equals(),
                    syntax.NotEquals(),
                ]

                if op in comparison_operators:
                    # E.g. `==` of two longs returns an int
                    return expr.set_type(syntax.Int())

                return expr.set_type(common_type)

            case syntax.Assignment(lhs, rhs, op):
                name = lhs.name
                if isinstance(self.symbols[name].type, syntax.Func):
                    self.error(f'Cannot assign to a function {name}')
                lhs = self.typecheck_expr(lhs)

                if op is not None:
                    # This logic only works because the assign target has no
                    # side effects (e.g. it isn't `a[i++]`) so it can safely be
                    # duplicated
                    rhs = syntax.Binary(op, lhs, rhs)
                    op = None

                rhs = self.typecheck_expr(rhs)
                left_type = lhs.expr_type
                assert(left_type is not None)
                converted_rhs = self.convert_to(rhs, left_type)
                expr = syntax.Assignment(lhs, converted_rhs, op)
                return expr.set_type(left_type)

            case syntax.Conditional(condition, t, e):
                condition = self.typecheck_expr(condition)
                t = self.typecheck_expr(t)
                e = self.typecheck_expr(e)

                common_type = self.get_common_type(t.expr_type, e.expr_type)
                converted_t = self.convert_to(t, common_type)
                converted_e = self.convert_to(e, common_type)

                expr = syntax.Conditional(condition, converted_t, converted_e)
                return expr.set_type(common_type)

            case syntax.Cast(target_type, e):
                e = self.typecheck_expr(e)
                expr = syntax.Cast(target_type, e)
                return expr.set_type(target_type)

            case _:
                raise Exception(f'Unhandled type of expression {expr}')

    def convert_to(self, expression, target_type):
        assert(expression.expr_type is not None)
        if expression.expr_type == target_type:
            return expression
        cast_expr = syntax.Cast(target_type, expression)
        cast_expr.set_type(target_type)
        return cast_expr

    def get_common_type(self, t1, t2):
        if t1 == t2:
            return t1
        if syntax.Double() in [t1, t2]:
            return syntax.Double()
        if typeconversion.type_size(t1) == typeconversion.type_size(t2):
            if typeconversion.is_signed(t1):
                return t2
            else:
                return t1
        if typeconversion.type_size(t1) > typeconversion.type_size(t2):
            return t1
        else:
            return t2
        raise Exception(f'Unhandled combination of types {t1} and {t2}')

    def typecheck_for_init(self, init: syntax.ForInit):
        match init:
            case syntax.InitDecl(decl):
                if isinstance(decl, syntax.VarDeclaration):
                    if decl.storage_class is not None:
                        self.error('variables defined in a for loop init cannot have a storage class')
                stmt = self.typecheck_statement(decl)
                return syntax.InitDecl(stmt)
            case syntax.InitExp(expr):
                if expr:
                    expr = self.typecheck_expr(expr)
                return syntax.InitExp(expr)
            case None:
                return None
            case _:
                raise Exception(f'invalid type for ForInit {init}')
