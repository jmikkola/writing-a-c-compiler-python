import typing
from collections import defaultdict
from collections import namedtuple

import syntax
import symbol
from errors import ValidationError, TypeError
import typeconversion


def validate(program: syntax.Program) -> typing.Tuple[syntax.Program, dict, dict]:
    program = IdentifierResolution().validate(program)
    program = LabelValidator().validate(program)
    program = LoopLabels().validate(program)
    typecheck = Typecheck()
    program, symbols, types = typecheck.typecheck(program)
    return (program, symbols, types)


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


class StructEntry(namedtuple('StructEntry', ['new_tag', 'is_struct', 'from_current_scope'])):
    ''' for IdentifierResolution to track structure and union names '''
    def mark_old(self):
        return StructEntry(self.new_tag, self.is_struct, False)

    @classmethod
    def struct(cls, new_tag):
        return StructEntry(new_tag, True, True)

    @classmethod
    def union(cls, new_tag):
        return StructEntry(new_tag, False, True)


def copy_struct_map(struct_map: dict) -> dict:
    return {
        name: entry.mark_old()
        for (name, entry) in struct_map.items()
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
        struct_map = {}
        declarations = [
            self.validate_declaration(d, identifier_map, struct_map)
            for d in program.declarations
        ]
        return syntax.Program(declarations)

    def validate_declaration(self, decl: syntax.Declaration, identifier_map, struct_map):
        match decl:
            case syntax.FuncDeclaration():
                identifier_map[decl.name] = MapEntry.for_name(decl.name, has_linkage=True)
                return self.validate_function(decl, identifier_map, struct_map)
            case syntax.VarDeclaration():
                identifier_map[decl.name] = MapEntry.for_name(decl.name, has_linkage=True)
                return self.validate_variable(decl, struct_map)
            case syntax.StructDeclaration():
                return self.validate_struct(decl, struct_map)
            case syntax.UnionDeclaration():
                return self.validate_union(decl, struct_map)
            case _:
                raise Exception(f'unhandled kind of declaration {decl}')

    def validate_variable(self, variable: syntax.VarDeclaration, struct_map):
        type = self.resolve_type(variable.var_type, struct_map)
        return syntax.VarDeclaration(
            variable.name,
            variable.init,
            type,
            variable.storage_class
        )

    def validate_struct(self, struct: syntax.StructDeclaration, struct_map):
        prev_entry = struct_map.get(struct.tag)
        if prev_entry is None or not prev_entry.from_current_scope:
            unique_tag = self.make_unique(struct.tag)
            struct_map[struct.tag] = StructEntry.struct(unique_tag)
        else:
            if not prev_entry.is_struct:
                self.error(f'cannot redeclare a union as a struct: {struct.tag}')
            unique_tag = prev_entry.new_tag

        processed_members = []
        for member in (struct.fields or []):
            processed_type = self.resolve_type(member.type, struct_map)
            processed_member = syntax.StructField(processed_type, member.name)
            processed_members.append(processed_member)
        return syntax.StructDeclaration(unique_tag, processed_members)

    def validate_union(self, union: syntax.UnionDeclaration, struct_map):
        prev_entry = struct_map.get(union.tag)
        if prev_entry is None or not prev_entry.from_current_scope:
            unique_tag = self.make_unique(union.tag)
            struct_map[union.tag] = StructEntry.union(unique_tag)
        else:
            if prev_entry.is_struct:
                self.error(f'cannot redeclare a struct as a union: {union.tag}')
            unique_tag = prev_entry.new_tag

        processed_members = []
        for member in (union.fields or []):
            processed_type = self.resolve_type(member.type, struct_map)
            processed_member = syntax.UnionField(processed_type, member.name)
            processed_members.append(processed_member)
        return syntax.UnionDeclaration(unique_tag, processed_members)

    def resolve_type(self, type_spec: syntax.Type, struct_map):
        match type_spec:
            case syntax.Struct(tag):
                if tag in struct_map:
                    entry = struct_map[tag]
                    if not entry.is_struct:
                        self.error(f'cannot refer to a union type as a struct: {tag}')
                    return syntax.Struct(entry.new_tag)
                else:
                    self.error(f'no struct declared with tag: {tag}')
            case syntax.Union(tag):
                if tag in struct_map:
                    entry = struct_map[tag]
                    if entry.is_struct:
                        self.error(f'cannot refer to a struct type as a union: {tag}')
                    return syntax.Union(entry.new_tag)
                else:
                    self.error(f'no union declared with tag: {tag}')
            case syntax.Pointer(pointed):
                resolved = self.resolve_type(pointed, struct_map)
                return syntax.Pointer(resolved)
            case syntax.Array(elem_t, size):
                resolved = self.resolve_type(elem_t, struct_map)
                return syntax.Array(resolved, size)
            case syntax.Func(params, ret):
                params = [
                    self.resolve_type(param, struct_map)
                    for param in params
                ]
                ret = self.resolve_type(ret, struct_map)
                return syntax.Func(params, ret)
            case _:
                return type_spec

    def validate_function(self, function: syntax.FuncDeclaration, identifier_map, struct_map):
        name = function.name
        type = function.fun_type

        if name in identifier_map:
            prev_entry = identifier_map[name]
            if prev_entry.from_current_scope and not prev_entry.has_linkage:
                self.error(f'duplicate declaration of {name}')
        identifier_map[name] = MapEntry.for_name(name, has_linkage=True)

        type = self.resolve_type(type, struct_map)

        inner_identifier_map = copy_identifier_map(identifier_map)
        inner_struct_map = copy_struct_map(struct_map)

        new_params = []
        for param in function.params:
            new_params.append(self.resolve_param(param, inner_identifier_map))

        body = function.body
        if body:
            block_items = [
                self.validate_statements(b, inner_identifier_map, inner_struct_map)
                for b in body.block_items
            ]
            body = syntax.Block(block_items)
        return syntax.FuncDeclaration(
            name,
            new_params,
            body,
            type,
            function.storage_class
        )

    def resolve_param(self, param, identifier_map):
        if param in identifier_map and identifier_map[param].from_current_scope:
            self.error(f'{param} already declared')
        new_name = self.make_unique(param)
        identifier_map[param] = MapEntry.for_name(new_name)
        return new_name

    def validate_block(self, block: syntax.Block, identifier_map: dict, struct_map: dict):
        inner_identifier_map = copy_identifier_map(identifier_map)
        inner_struct_map = copy_struct_map(struct_map)
        block_items = [
            self.validate_statements(b, inner_identifier_map, inner_struct_map)
            for b in block.block_items
        ]
        return syntax.Block(block_items)

    def validate_statements(self, block_item: syntax.BlockItem, identifier_map: dict, struct_map: dict):
        match block_item:
            case syntax.VarDeclaration():
                return self.validate_block_scope_variable(block_item, identifier_map, struct_map)
            case syntax.FuncDeclaration(name, _, body, storage_class):
                if body is not None:
                    self.error('function definitions not allowed inside another function')
                if storage_class == syntax.Static():
                    self.error('function declarations inside a block cannot be static')
                return self.validate_function(block_item, identifier_map, struct_map)
            case syntax.StructDeclaration():
                return self.validate_struct(block_item, struct_map)
            case syntax.UnionDeclaration():
                return self.validate_union(block_item, struct_map)
            case syntax.Return(expr):
                if expr is not None:
                    expr = self.resolve_expr(expr, identifier_map, struct_map)
                return syntax.Return(expr)
            case syntax.ExprStmt(expr):
                expr = self.resolve_expr(expr, identifier_map, struct_map)
                return syntax.ExprStmt(expr)
            case syntax.NullStatement():
                return block_item
            case syntax.IfStatement(test, t, e):
                test = self.resolve_expr(test, identifier_map, struct_map)
                t = self.validate_statements(t, identifier_map, struct_map)
                if e:
                    e = self.validate_statements(e, identifier_map, struct_map)
                return syntax.IfStatement(test, t, e)
            case syntax.Goto(_):
                return block_item
            case syntax.LabeledStmt(label, stmt):
                stmt = self.validate_statements(stmt, identifier_map, struct_map)
                return syntax.LabeledStmt(label, stmt)
            case syntax.Compound(block):
                block = self.validate_block(block, identifier_map, struct_map)
                return syntax.Compound(block)
            case syntax.Break(_):
                return block_item
            case syntax.Continue(_):
                return block_item
            case syntax.While(test, body, loop_label):
                test = self.resolve_expr(test, identifier_map, struct_map)
                body = self.validate_statements(body, identifier_map, struct_map)
                return syntax.While(test, body, loop_label)
            case syntax.DoWhile(body, test, loop_label):
                body = self.validate_statements(body, identifier_map, struct_map)
                test = self.resolve_expr(test, identifier_map, struct_map)
                return syntax.DoWhile(body, test, loop_label)
            case syntax.For(init, condition, post, body, loop_label):
                inner_identifier_map = copy_identifier_map(identifier_map)
                init = self.resolve_for_init(init, inner_identifier_map, struct_map)
                if condition:
                    condition = self.resolve_expr(condition, inner_identifier_map, struct_map)
                if post:
                    post = self.resolve_expr(post, inner_identifier_map, struct_map)
                body = self.validate_statements(body, inner_identifier_map, struct_map)
                return syntax.For(init, condition, post, body, loop_label)
            case syntax.Switch(condition, body, switch_label, case_values):
                condition = self.resolve_expr(condition, identifier_map, struct_map)
                body = self.validate_statements(body, identifier_map, struct_map)
                return syntax.Switch(condition, body, switch_label, case_values)
            case syntax.Case(value, stmt, switch_label):
                if stmt:
                    stmt = self.validate_statements(stmt, identifier_map, struct_map)
                return syntax.Case(value, stmt, switch_label)
            case syntax.Default(stmt, switch_label):
                if stmt:
                    stmt = self.validate_statements(stmt, identifier_map, struct_map)
                return syntax.Default(stmt, switch_label)
            case _:
                raise Exception(f'unhandled type of block item {block_item}')

    def validate_block_scope_variable(self, var: syntax.VarDeclaration, identifier_map, struct_map):
        name = var.name
        is_extern = var.storage_class == syntax.Extern()
        var_type = self.resolve_type(var.var_type, struct_map)

        if name in identifier_map:
            prev_entry = identifier_map[name]
            if prev_entry.from_current_scope:
                if not (prev_entry.has_linkage and is_extern):
                    self.error(f'conflicting declarations for {name}')

        if is_extern:
            identifier_map[name] = MapEntry.for_name(name, has_linkage=True)
            return syntax.VarDeclaration(name, var.init, var_type, var.storage_class)
        else:
            unique_name = self.make_unique(name)
            identifier_map[name] = MapEntry.for_name(unique_name)

            init = var.init
            if init:
                init = self.resolve_init(init, identifier_map, struct_map)

            return syntax.VarDeclaration(unique_name, init, var_type, var.storage_class)

    def resolve_init(self, init: syntax.Initializer, identifier_map, struct_map):
        match init:
            case syntax.SingleInit(expr):
                expr = self.resolve_expr(expr, identifier_map, struct_map)
                return syntax.SingleInit(expr)
            case syntax.CompoundInit(initializers):
                initializers = [
                    self.resolve_init(i, identifier_map, struct_map)
                    for i in initializers
                ]
                return syntax.CompoundInit(initializers)
            case _:
                raise Exception(f'unhandled type of initializer {init}')

    def resolve_for_init(self, init: syntax.ForInit, identifier_map: dict, struct_map: dict):
        match init:
            case syntax.InitDecl(decl):
                decl = self.validate_statements(decl, identifier_map, struct_map)
                return syntax.InitDecl(decl)
            case syntax.InitExp(exp):
                if exp:
                    exp = self.resolve_expr(exp, identifier_map, struct_map)
                return syntax.InitExp(exp)
            case _:
                raise Exception(f'invalid type for ForInit {init}')


    def resolve_expr(self, expr: syntax.Expression, identifier_map: dict, struct_map: dict):
        match expr:
            case syntax.Constant(_):
                return expr
            case syntax.String(_):
                return expr
            case syntax.Variable(name):
                if name not in identifier_map:
                    self.error(f'undefined variable {name}')
                return syntax.Variable(identifier_map[name].new_name)
            case syntax.Unary(op, expr):
                if self.is_modifying_operator(op) and not is_lvalue(expr):
                    self.error(f'invalid to apply {op} to {expr}')
                expr = self.resolve_expr(expr, identifier_map, struct_map)
                return syntax.Unary(op, expr)
            case syntax.Postfix(expr, op):
                if self.is_modifying_operator(op) and not is_lvalue(expr):
                    self.error(f'invalid to apply {op} to {expr}')
                expr = self.resolve_expr(expr, identifier_map, struct_map)
                return syntax.Postfix(expr, op)
            case syntax.Binary(op, left, right):
                left = self.resolve_expr(left, identifier_map, struct_map)
                right = self.resolve_expr(right, identifier_map, struct_map)
                return syntax.Binary(op, left, right)
            case syntax.Assignment(lhs, rhs, op):
                lhs = self.resolve_expr(lhs, identifier_map, struct_map)
                rhs = self.resolve_expr(rhs, identifier_map, struct_map)
                return syntax.Assignment(lhs, rhs, op)
            case syntax.Conditional(test, t, e):
                test = self.resolve_expr(test, identifier_map, struct_map)
                t = self.resolve_expr(t, identifier_map, struct_map)
                e = self.resolve_expr(e, identifier_map, struct_map)
                return syntax.Conditional(test, t, e)
            case syntax.Call(func_name, arguments):
                if func_name not in identifier_map:
                    self.error(f'calling an undefined function {func_name}')
                new_func_name = identifier_map[func_name].new_name
                new_args = [self.resolve_expr(arg, identifier_map, struct_map) for arg in arguments]
                return syntax.Call(new_func_name, new_args)
            case syntax.Cast(target_type, expr):
                target_type = self.resolve_type(target_type, struct_map)
                expr = self.resolve_expr(expr, identifier_map, struct_map)
                return syntax.Cast(target_type, expr)
            case syntax.Dereference(expr):
                expr = self.resolve_expr(expr, identifier_map, struct_map)
                return syntax.Dereference(expr)
            case syntax.AddrOf(expr):
                expr = self.resolve_expr(expr, identifier_map, struct_map)
                return syntax.AddrOf(expr)
            case syntax.Subscript(left, right):
                left = self.resolve_expr(left, identifier_map, struct_map)
                right = self.resolve_expr(right, identifier_map, struct_map)
                return syntax.Subscript(left, right)
            case syntax.Dot(expr, member):
                expr = self.resolve_expr(expr, identifier_map, struct_map)
                return syntax.Dot(expr, member)
            case syntax.Arrow(expr, member):
                expr = self.resolve_expr(expr, identifier_map, struct_map)
                return syntax.Arrow(expr, member)
            case syntax.SizeOf(expr):
                expr = self.resolve_expr(expr, identifier_map, struct_map)
                return syntax.SizeOf(expr)
            case syntax.SizeOfT(type):
                type = self.resolve_type(type, struct_map)
                return syntax.SizeOfT(type)
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
            case syntax.StructDeclaration():
                return decl
            case syntax.UnionDeclaration():
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
                 syntax.StructDeclaration() | \
                 syntax.UnionDeclaration() | \
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
            case syntax.StructDeclaration():
                return decl
            case syntax.UnionDeclaration():
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
                 syntax.StructDeclaration() | \
                 syntax.UnionDeclaration() | \
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

## Entries in the type table:

class StructType(namedtuple('StructType', ['alignment', 'size', 'members'])):
    ''' a defined struct type '''
    def get_member(self, name):
        for m in self.members:
            if m.name == name:
                return m


class StructMember(namedtuple('StructMember', ['name', 'type', 'offset'])):
    ''' a field in a struct type '''
    pass


class UnionType(namedtuple('UnionType', ['alignment', 'size', 'members'])):
    ''' a defined union type '''
    def get_member(self, name):
        for m in self.members:
            if m.name == name:
                return m


class UnionMember(namedtuple('UnionMember', ['name', 'type', 'offset'])):
    ''' a field in a union type '''
    # Offset is always zero, but is here just to make code in the next pass simpler
    pass


class Typecheck:
    def __init__(self):
        self.symbols = {}
        self.types = {}
        self.current_return_type = None
        self._switch_condition_types = {}
        self._n_temp = 0

    def new_temp_var(self, name):
        result = f'tmp.{name}.{self._n_temp}'
        self._n_temp += 1
        return result

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

        return program, self.symbols, self.types

    def typecheck_decl(self, decl: syntax.Declaration):
        match decl:
            case syntax.FuncDeclaration():
                return self.typecheck_func_decl(decl)
            case syntax.VarDeclaration():
                return self.typecheck_var_decl_file_scope(decl)
            case syntax.StructDeclaration():
                self.check_struct_decl(decl)
                return decl
            case syntax.UnionDeclaration():
                self.check_union_decl(decl)
                return decl
            case _:
                raise Exception(f'unhandled kind of declaration {decl}')

    def check_struct_decl(self, decl: syntax.StructDeclaration):
        if not decl.fields:
            # This is a declaration not a definition, so ignore it
            return

        self.validate_struct_decl(decl)

        member_entries = []
        struct_size = 0
        struct_alignment = 1
        for field in decl.fields:
            member_alignment = self.get_alignment(field.type)
            member_offset = round_up(struct_size, member_alignment)
            entry = StructMember(field.name, field.type, member_offset)
            member_entries.append(entry)
            struct_alignment = max(struct_alignment, member_alignment)
            struct_size = member_offset + self.get_size(field.type)

        struct_size = round_up(struct_size, struct_alignment)
        struct_entry = StructType(struct_alignment, struct_size, member_entries)
        self.types[decl.tag] = struct_entry

    def check_union_decl(self, decl: syntax.UnionDeclaration):
        if not decl.fields:
            # This is a declaration not a definition, so ignore it
            return

        self.validate_union_decl(decl)

        member_entries = []
        union_size = 0
        union_alignment = 1
        for field in decl.fields:
            member_size = self.get_size(field.type)
            union_size = max(union_size, member_size)
            member_alignment = self.get_alignment(field.type)
            union_alignment = max(union_alignment, member_alignment)
            entry = UnionMember(field.name, field.type, 0)
            member_entries.append(entry)

        union_size = round_up(union_size, union_alignment)
        union_entry = UnionType(union_alignment, union_size, member_entries)
        self.types[decl.tag] = union_entry

    def get_alignment(self, type: syntax.Type) -> int:
        ''' this is specifically for the alignment of types, not variables '''
        match type:
            case syntax.Array(elem_type, _size):
                # This is a different alignment rule than variables of array types,
                # where any array 16 bytes or larger is 16-byte aligned.
                return self.get_alignment(elem_type)
            case _:
                return typeconversion.alignment_of(type, self.types)

    def get_size(self, type: syntax.Type) -> int:
        return typeconversion.type_size(type, self.types)

    def validate_struct_decl(self, decl: syntax.StructDeclaration):
        tag = decl.tag
        if tag in self.types:
            self.error(f'multiple definitions in the same scope of struct: {tag}')

        field_names_used = set()
        for field in decl.fields:
            if field.name in field_names_used:
                self.error(f'duplicate struct field {field.name} in struct {tag}')
            field_names_used.add(field.name)
            self.validate_type_specifier(field.type)

    def validate_union_decl(self, decl: syntax.UnionDeclaration):
        tag = decl.tag
        if tag in self.types:
            self.error(f'multiple definitions in the same scope of union: {tag}')

        field_names_used = set()
        for field in decl.fields:
            if field.name in field_names_used:
                self.error(f'duplicate union field {field.name} in union {tag}')
            field_names_used.add(field.name)
            self.validate_type_specifier(field.type)

    def typecheck_func_decl(self, f: syntax.FuncDeclaration, in_block=False):
        func_type = f.fun_type

        if self.is_array(func_type.ret):
            self.error('functions cannot return arrays')
        for t in func_type.params:
            if is_void(t):
                self.error('cannot have void typed arguments to functions')
        if f.body is not None:
            # Function declarations can have incomplete types, but not function
            # definitions
            self.validate_type_specifier(func_type)
        else:
            # Even if they don't have bodies, they still can't use invalid types
            # like arrays of void
            self.validate_non_void_type(func_type)

        # Array type arguments are implicitly converted to pointers
        adjusted_params = []
        for t in func_type.params:
            match t:
                case syntax.Array(elem_t, size):
                    adjusted_type = syntax.Pointer(elem_t)
                    adjusted_params.append(adjusted_type)
                case _:
                    adjusted_params.append(t)

        func_type = syntax.Func(adjusted_params, func_type.ret)

        has_body = f.body is not None
        if has_body:
            self.current_return_type = func_type.ret
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
        if is_void(v.var_type):
            self.error('cannot have variables of type void')
        if not (v.storage_class == syntax.Extern() and v.init is None):
            # only extern variables with no initializer can use incomplete types
            self.validate_type_specifier(v.var_type)
        else:
            self.validate_non_void_type(v.var_type)

        initial_value = None
        match v.init:
            case syntax.SingleInit() | syntax.CompoundInit():
                initial_value = self.make_initial_value(v.var_type, v.init)
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
        if is_void(v.var_type):
            self.error('cannot have variables of type void')
        if not (v.storage_class == syntax.Extern() and v.init is None):
            # only extern variables with no initializer can use incomplete types
            self.validate_type_specifier(v.var_type)
        else:
            self.validate_non_void_type(v.var_type)

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
            initial_value = self.make_initial_value(v.var_type, v.init)
            attrs = symbol.StaticAttr(initial_value, False)
            self.symbols[v.name] = symbol.Symbol(v.var_type, attrs)

        else:
            self.symbols[v.name] = symbol.Symbol(v.var_type, symbol.LocalAttr())
            init = None
            if v.init is not None:
                init = self.typecheck_var_init(v.var_type, v.init)
                return syntax.VarDeclaration(v.name, init, v.var_type, v.storage_class)
        return v

    def typecheck_var_init(self, target_type: syntax.Type, init: syntax.Initializer):
        match (target_type, init):
            case (syntax.Struct(tag), syntax.CompoundInit(inits)):
                struct_def = self.types.get(tag)
                assert(struct_def)
                if len(inits) > len(struct_def.members):
                    self.error('too many elements in struction initializer')
                typechecked_list = []
                i = 0
                # Check and convert the initializers for each field
                for init in inits:
                    struct_member = struct_def.members[i]
                    typechecked_elem = self.typecheck_var_init(struct_member.type, init)
                    typechecked_list.append(typechecked_elem)
                    i += 1
                # Add zero initializers for any uninitialized fields
                while i < len(struct_def.members):
                    struct_member = struct_def.members[i]
                    typechecked_list.append(self.zero_initializer(struct_member.type))
                    i += 1
                return syntax.CompoundInit(typechecked_list).set_type(target_type)

            case (syntax.Union(tag), syntax.CompoundInit(inits)):
                union_def = self.types.get(tag)
                assert(union_def)
                if len(inits) != 1:
                    self.error('can only initialize a union with one initializer')
                union_member = union_def.members[0]
                init = inits[0]
                typechecked_elem = self.typecheck_var_init(union_member.type, init)
                return syntax.CompoundInit([typechecked_elem]).set_type(target_type)

            case (syntax.Array(elem_t, size), syntax.SingleInit(syntax.String(bytes))):
                if not self.is_character(elem_t):
                    self.error("can't initialize a non-character type with a string literal")
                if len(bytes) > size:
                    self.error("Too many characters in string literal")
                return init.set_type(target_type)

            case (_, syntax.SingleInit(e)):
                typechecked_expr = self.typecheck_and_convert(e)
                cast_expr = self.convert_by_assignment(typechecked_expr, target_type)
                return syntax.SingleInit(cast_expr).set_type(target_type)

            case (syntax.Array(elem_t, size), syntax.CompoundInit(inits)):
                if len(inits) > size:
                    self.error('wrong number of valies in initializer')
                typechecked_list = [
                    self.typecheck_var_init(elem_t, init)
                    for init in inits
                ]
                while len(typechecked_list) < size:
                    typechecked_list.append(self.zero_initializer(elem_t))
                return syntax.CompoundInit(typechecked_list).set_type(target_type)

            case _:
                self.error('cannot initialize a scalar object with a compound initializer')

    def zero_initializer(self, target_type):
        match target_type:
            case syntax.Array(elem_t, size):
                inner = self.zero_initializer(elem_t)
                values = [inner] * size
                return syntax.CompoundInit(values).set_type(target_type)
            case syntax.Struct(tag):
                struct_def = self.types.get(tag)
                assert(struct_def)
                values = [
                    self.zero_initializer(member.type)
                    for member in struct_def.members
                ]
                return syntax.CompoundInit(values).set_type(target_type)
            case syntax.Union(tag):
                union_def = self.types.get(tag)
                assert(union_def)
                value = self.zero_initializer(union_def.members[0].type)
                return syntax.CompoundInit([value]).set_type(target_type)
            case syntax.Char() | syntax.SChar():
                value = syntax.ConstInt(0)
            case syntax.UChar():
                value = syntax.ConstUInt(0)
            case syntax.Int():
                value = syntax.ConstInt(0)
            case syntax.UInt():
                value = syntax.ConstUInt(0)
            case syntax.Long():
                value = syntax.ConstLong(0)
            case syntax.ULong():
                value = syntax.ConstULong(0)
            case syntax.Double():
                value = syntax.ConstDouble(0.0)
            case syntax.Pointer():
                value = syntax.ConstULong(0)
            case _:
                raise Exception(f'unhandled initializer type {target_type}')
        constant = syntax.Constant(value)
        constant.set_type(target_type)
        return syntax.SingleInit(constant).set_type(target_type)

    def make_initial_value(self, target_type: syntax.Type, init: syntax.CompoundInit):
        static_values = self.make_static_values(target_type, init)
        return symbol.Initial(static_values)

    def make_static_values(self, target_type: syntax.Type, init: syntax.Initializer):
        ''' recurse down into any compound initializers and flatten the values '''
        match (target_type, init):
            ### Constant values ###
            case (syntax.Array(), syntax.SingleInit(syntax.Constant())):
                self.error(f'cannot use a single initializers for an array')
            case (syntax.Struct(), syntax.SingleInit(syntax.Constant())):
                self.error(f'cannot use a single initializers for a struct')
            case (syntax.Union(), syntax.SingleInit(syntax.Constant())):
                self.error(f'cannot use a single initializers for a union')
            case (_, syntax.SingleInit(syntax.Constant(const))):
                return [self.to_static_init(const.value, target_type)]

            ### String values ###
            case (syntax.Array(elem_t, size), syntax.SingleInit(syntax.String(bytes))):
                if not self.is_character(elem_t):
                    # Char, UChar, and SChar are allowed
                    self.error(f'cannot use a string to initialize an array of {elem_t}')
                if size < len(bytes):
                    self.error(f'string is too long for {target_type}')
                null_terminated = target_type.size > len(bytes)
                static_values = [symbol.StringInit(bytes, null_terminated)]
                if len(bytes) + 1 < size:
                    # +1 for the null terminator
                    padding_size = size - len(bytes) - 1
                    static_values.append(symbol.ZeroInit(padding_size))
                return static_values

            case (syntax.Pointer(syntax.Char()), syntax.SingleInit(syntax.String(bytes))):
                # UChar and SChar are not allowed here
                new_name = self.new_temp_var('str')
                string_value = symbol.Initial([symbol.StringInit(bytes, True)])
                self.symbols[new_name] = symbol.Symbol(
                    syntax.Array(syntax.Char(), len(bytes) + 1),
                    symbol.ConstantAttr(init=string_value)
                )
                return [symbol.PointerInit(new_name)]

            case (_, syntax.SingleInit(syntax.String(bytes))):
                self.error(f'invalid type for assigning a string: {target_type}')

            ### Any other single init ###
            case (_, syntax.SingleInit(expr)):
                self.error(f'non-constant initializer for a static variable: {expr}')

            ### Compound initializers ###
            case (syntax.Array(elem_t, size), syntax.CompoundInit(initializers)):
                if len(initializers) > size:
                    self.error(f'too many values in initializer for {target_type}')
                static_values = []
                for initializer in initializers:
                    static_values.extend(self.make_static_values(elem_t, initializer))
                if len(initializers) < size:
                    n_values_to_pad = size - len(initializers)
                    padding_size = self.get_size(elem_t)
                    static_values.append(symbol.ZeroInit(n_values_to_pad * padding_size))
                return static_values

            case (syntax.Struct(tag), syntax.CompoundInit(initializers)):
                struct_def = self.types.get(tag)
                assert(struct_def)
                members = struct_def.members
                if len(initializers) > len(members):
                    self.error(f'too many initializers for struct {tag}')

                static_values = []
                current_offset = 0
                i = 0
                for init_elem in initializers:
                    member = members[i]
                    if member.offset != current_offset:
                        static_values.append(symbol.ZeroInit(member.offset - current_offset))
                    static_values.extend(self.make_static_values(member.type, init_elem))
                    current_offset = member.offset + self.get_size(member.type)
                    i += 1
                if current_offset != struct_def.size:
                    static_values.append(symbol.ZeroInit(struct_def.size - current_offset))
                return static_values

            case (syntax.Union(tag), syntax.CompoundInit(initializers)):
                union_def = self.types.get(tag)
                assert(union_def)
                if len(initializers) != 1:
                    self.error('can only use one initializer for a union')
                member = union_def.members[0]
                init_elem = initializers[0]
                static_values = self.make_static_values(member.type, init_elem)
                remaining_bytes = union_def.size - self.get_size(member.type)
                if remaining_bytes > 0:
                    static_values.append(symbol.ZeroInit(remaining_bytes))
                return static_values

            case (_, syntax.CompoundInit()):
                self.error(f'cannot use a compound initializers for scalars')

            ### Other cases ###
            case (_, None):
                bytes = self.get_size(target_type)
                return [symbol.ZeroInit(bytes)]

            case (_, _):
                raise Exception(f'unhandled type of initializer {init}')

    def to_static_init(self, value, var_type):
        match var_type:
            case syntax.Long():
                value = typeconversion.constant_to_long(value)
                return symbol.LongInit(value)
            case syntax.Int():
                value = typeconversion.constant_to_int(value)
                return symbol.IntInit(value)
            case syntax.Char() | syntax.SChar():
                value = typeconversion.constant_to_byte(value)
                return symbol.CharInit(value)
            case syntax.ULong():
                value = typeconversion.constant_to_long(value, unsigned=True)
                return symbol.ULongInit(value)
            case syntax.UInt():
                value = typeconversion.constant_to_int(value, unsigned=True)
                return symbol.UIntInit(value)
            case syntax.UChar():
                value = typeconversion.constant_to_byte(value, unsigned=True)
                return symbol.UCharInit(value)
            case syntax.Double():
                if isinstance(value, int):
                    value = float(value)
                return symbol.DoubleInit(value)
            case syntax.Pointer():
                if value != 0:
                    self.error(f'cannot initialize a pointer to a non-zero constant: {value}')
                return symbol.ULongInit(0)
            case _:
                raise Exception(f'unhandled type for static constant {var_type}')

    def is_null_pointer_constant(self, e: syntax.Expression) -> bool:
        match e:
            case syntax.Constant(c):
                match c:
                    case syntax.ConstInt(0):
                        return True
                    case syntax.ConstUInt(0):
                        return True
                    case syntax.ConstLong(0):
                        return True
                    case syntax.ConstULong(0):
                        return True
                    case _:
                        return False
            case _:
                return False

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
            case syntax.Return(None):
                if not is_void(self.current_return_type):
                    self.error(f'returning no value from a function with a return type: {self.current_return_type}')
                return syntax.Return(None)
            case syntax.Return(expr):
                if is_void(self.current_return_type):
                    self.error(f'returning a value from a void function: {block_item}')
                expr = self.typecheck_and_convert(expr)
                expr = self.convert_by_assignment(expr, self.current_return_type)
                return syntax.Return(expr)
            case syntax.ExprStmt(expr):
                expr = self.typecheck_and_convert(expr)
                return syntax.ExprStmt(expr)
            case syntax.Goto(_) | \
                 syntax.NullStatement():
                return block_item
            case syntax.IfStatement(test, t, e):
                test = self.typecheck_and_convert(test)
                if not is_scalar(test.expr_type):
                    self.error('the test of an if statement must have a scalar type')
                t = self.typecheck_statement(t)
                if e:
                    e = self.typecheck_statement(e)
                return syntax.IfStatement(test, t, e)
            case syntax.While(test, body, loop_label):
                test = self.typecheck_and_convert(test)
                if not is_scalar(test.expr_type):
                    self.error('the test of an while loop must have a scalar type')
                body = self.typecheck_statement(body)
                return syntax.While(test, body, loop_label)
            case syntax.DoWhile(body, test, loop_label):
                test = self.typecheck_and_convert(test)
                if not is_scalar(test.expr_type):
                    self.error('the test of a do-while loop must have a scalar type')
                body = self.typecheck_statement(body)
                return syntax.DoWhile(body, test, loop_label)
            case syntax.For(init, condition, post, body, loop_label):
                init = self.typecheck_for_init(init)
                if condition:
                    condition = self.typecheck_and_convert(condition)
                    if not is_scalar(condition.expr_type):
                        self.error('the condition of a for loop must have a scalar type')
                if post:
                    post = self.typecheck_and_convert(post)
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
                # The type without the conversion that decays arrays to pointers
                raw_condition_type = self.typecheck_expr(condition).expr_type
                if self.is_array(raw_condition_type):
                    self.error('cannot switch on an array value')
                condition = self.typecheck_and_convert(condition)
                condition_type = condition.expr_type
                if not is_scalar(condition_type):
                    self.error('the condition of a switch statement must have a scalar type')
                # Promote characters to integers in switch statements
                if self.is_character(condition_type):
                    condition_type = syntax.Int()
                    condition = self.convert_to(condition, condition_type)
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
            case syntax.StructDeclaration():
                self.check_struct_decl(block_item)
                return block_item
            case syntax.UnionDeclaration():
                self.check_union_decl(block_item)
                return block_item
            case _:
                raise Exception(f'unhandled type of block item {block_item}')

    def convert_case_values(self, case_values: set, target_type: syntax.Type) -> set:
        if self.is_character(target_type):
            target_type = syntax.Int()
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
            case syntax.ConstChar(n):
                return self.convert_integer(n, target_type)
            case syntax.ConstInt(n):
                return self.convert_integer(n, target_type)
            case syntax.ConstLong(n):
                return self.convert_integer(n, target_type)
            case syntax.ConstUChar(n):
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
            case syntax.Char() | syntax.SChar():
                value = typeconversion.constant_to_byte(value)
                return syntax.ConstChar(value)
            case syntax.ULong():
                value = typeconversion.constant_to_long(value, unsigned=True)
                return syntax.ConstULong(value)
            case syntax.UInt():
                value = typeconversion.constant_to_int(value, unsigned=True)
                return syntax.ConstUInt(value)
            case syntax.UChar():
                value = typeconversion.constant_to_byte(value, unsigned=True)
                return syntax.ConstUChar(value)
            case _:
                raise Exception(f'unhandled type for constant {target_type}')

    def typecheck_and_convert(self, expr: syntax.Expression):
        typed = self.typecheck_expr(expr)
        assert(typed.expr_type is not None)
        match typed.expr_type:
            case syntax.Array(elem_type, _size):
                # Insert an implicit dereference when the expression's type is array
                addr_expr = syntax.AddrOf(typed)
                return addr_expr.set_type(syntax.Pointer(elem_type))
            case syntax.Struct(tag):
                if tag not in self.types:
                    self.error(f'invalid use of incomplete structure type: {tag}')
                return typed
            case syntax.Union(tag):
                if tag not in self.types:
                    self.error(f'invalid use of incomplete union type: {tag}')
                return typed
            case _:
                return typed

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

            case syntax.String(bytes):
                # Add 1 to the array size for the space of the null terminator byte
                array_type = syntax.Array(syntax.Char(), 1 + len(bytes))
                return expr.set_type(array_type)

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
                    arg = self.typecheck_and_convert(arg)
                    converted_arg = self.convert_by_assignment(arg, param_type)
                    converted_arguments.append(converted_arg)

                expr = syntax.Call(name, converted_arguments)
                return expr.set_type(symbol_type.ret)

            case syntax.Unary(op, e):
                was_string = isinstance(e, syntax.String)
                raw_type = self.typecheck_expr(e).expr_type
                e = self.typecheck_and_convert(e)
                if not is_scalar(e.expr_type):
                    self.error(f'Cannot apply unary op {op} to a non-scalar type {e.expr_type}')
                if e.expr_type == syntax.Double():
                    if op == syntax.UnaryInvert():
                        self.error(f'Cannot apply unary op {op} to a double')
                if isinstance(e.expr_type, syntax.Pointer):
                    if op == syntax.UnaryNegate() or op == syntax.UnaryInvert():
                        self.error(f'invalid unary operator for a pointer: {op}')
                if is_void_pointer(e.expr_type):
                    self.error(f'cannot apply op to a void pointer: {op}')
                if isinstance(raw_type, syntax.Array):
                    if was_string:
                        # Unary negate and invert are OK for strings
                        if op == syntax.UnaryIncrement() or op == syntax.UnaryDecrement():
                            self.error(f'cannot apply unary operator to strings {op}')
                    else:
                        self.error(f'cannot apply unary operators to arrays {op}')
                # Cast characters to ints for some operations
                if self.is_character(e.expr_type):
                    if op == syntax.UnaryNegate() or op == syntax.UnaryInvert():
                        e = self.convert_to(e, syntax.Int())
                expr = syntax.Unary(op, e)
                if op == syntax.UnaryNot():
                    return expr.set_type(syntax.Int())
                else:
                    return expr.set_type(e.expr_type)

            case syntax.Postfix(e, op):
                raw_type = self.typecheck_expr(e).expr_type
                if isinstance(raw_type, syntax.Array):
                    self.error(f'cannot apply postfix operators to arrays: {op}')
                e = self.typecheck_and_convert(e)
                if not is_scalar(e.expr_type):
                    self.error(f'cannot apply postfix operators to non-scalar types: {op}')
                if is_void_pointer(e.expr_type):
                    self.error(f'cannot apply postifx operators to void pointers: {op}')
                expr = syntax.Postfix(e, op)
                return expr.set_type(e.expr_type)

            case syntax.Binary():
                return self.typecheck_binary(expr)

            case syntax.Assignment(lhs, rhs, op):
                lhs = self.typecheck_and_convert(lhs)
                if not is_lvalue(lhs):
                    self.error(f'invalid target for assignment: {lhs}')

                left_type = lhs.expr_type
                assert(left_type is not None)

                if is_void(left_type):
                    self.error('cannot assign to void')
                if isinstance(left_type, syntax.Func):
                    self.error(f'Cannot assign to a function {name}')

                if op is None:
                    # Handle an assignment without an operator (e.g. `x = foo()`)
                    rhs = self.typecheck_and_convert(rhs)
                    converted_rhs = self.convert_by_assignment(rhs, left_type)
                    expr = syntax.Assignment(lhs, converted_rhs, None)
                    return expr.set_type(left_type)

                # The rest of this handles assignments with operators,
                # e.g. `x += y`.

                if isinstance(left_type, syntax.Pointer):
                    # in e.g. `x |= foo()`, the expression is invalid if `x` is
                    # some kind of pointer
                    self.check_pointer_op(op)

                match lhs:
                    case syntax.Variable() | syntax.Dereference(syntax.Variable()):
                        # Rewrite simple expressions in a way that duplicates
                        # the lhs. Only do this when this doesn't create extra
                        # work (and has no risk of duplicating side effects).
                        new_rhs = syntax.Binary(op, lhs, rhs)
                        typed_rhs = self.typecheck_and_convert(new_rhs)
                        converted_rhs = self.convert_by_assignment(typed_rhs, left_type)
                        expr = syntax.Assignment(lhs, converted_rhs, None)
                        return expr.set_type(left_type)
                    case syntax.Dereference(inner):
                        # When the lhs is more complicated (e.g. `**pptr`,
                        # `*foo()`, etc), evaluate it up to the point that we
                        # know the address the pointer points to as a separate
                        # expression.
                        # This converts
                        #   *<expr1> <op>= <expr2>
                        # into
                        #   tmp.ptr.0 = <expr1>
                        #   *tmp.ptr.0 = *tpm.ptr.0 <op> <expr2>
                        ptr_value_name = self.new_temp_var('ptr')
                        ptr_var = syntax.Variable(ptr_value_name)

                        inner = self.typecheck_and_convert(inner)
                        self.symbols[ptr_value_name] = symbol.Symbol(inner.expr_type, symbol.LocalAttr())

                        # get_ptr is the `tmp.ptr.0 = <expr1>` part
                        get_ptr = syntax.Assignment(ptr_var, inner, None)
                        get_ptr = self.typecheck_and_convert(get_ptr)

                        # assign is the `*tmp.ptr.0 = *tmp.ptr.0 <op> <expr2>` part
                        dereference = syntax.Dereference(ptr_var)
                        new_rhs = syntax.Binary(op, dereference, rhs)
                        assign = syntax.Assignment(dereference, new_rhs, None)
                        assign = self.typecheck_and_convert(assign)

                        expr = syntax.Sequence(get_ptr, assign)
                        return expr.set_type(left_type)
                    case syntax.Subscript():
                        # E.g. a[1] <op>= <expr> or 1[a] <op>= <expr>
                        # Convert it to:
                        #   tmp.ptr.0 = &a[1]
                        #   *tmp.ptr.0 = *tmp.ptr.0 <op> <expr>
                        ptr_value_name = self.new_temp_var('ptr')
                        ptr_var = syntax.Variable(ptr_value_name)

                        access = syntax.AddrOf(lhs).set_type(syntax.Pointer(lhs.expr_type))
                        ptr_type = access.expr_type
                        self.symbols[ptr_value_name] = symbol.Symbol(ptr_type, symbol.LocalAttr())

                        # get_offset is the fist statement, `tmp.ptr.0 = &a[1]`
                        ptr_var = ptr_var.set_type(ptr_type)
                        get_ptr = syntax.Assignment(ptr_var, access, None)
                        get_ptr = get_ptr.set_type(ptr_type)

                        # assign is the second statement, `*tmp.ptr.0 = *tmp.ptr.0 <op> <expr>`
                        dereference = syntax.Dereference(ptr_var)
                        new_rhs = syntax.Binary(op, dereference, rhs)
                        assign = syntax.Assignment(dereference, new_rhs, None)
                        assign = self.typecheck_and_convert(assign)

                        # Return one expression that performs both statements
                        expr = syntax.Sequence(get_ptr, assign)
                        return expr.set_type(left_type)
                    case _:
                        self.error(f'invalid left hand side for assignment: {lhs}')

            case syntax.Conditional(condition, t, e):
                condition = self.typecheck_and_convert(condition)
                t = self.typecheck_and_convert(t)
                e = self.typecheck_and_convert(e)

                if not is_scalar(condition.expr_type):
                    self.error('The condition of a ternary expression must have a scalar type')

                t_type = t.expr_type
                e_type = e.expr_type
                if is_void(t_type) and is_void(e_type):
                    common_type = t_type
                elif isinstance(t_type, syntax.Pointer) or isinstance(e_type, syntax.Pointer):
                    common_type = self.get_common_pointer_type(t, e)
                else:
                    common_type = self.get_common_type(t_type, e_type)

                if is_struct_or_union(common_type) or is_void(common_type):
                    converted_t = t
                    converted_e = e
                else:
                    converted_t = self.convert_to(t, common_type)
                    converted_e = self.convert_to(e, common_type)

                expr = syntax.Conditional(condition, converted_t, converted_e)
                return expr.set_type(common_type)

            case syntax.Cast(target_type, e):
                e = self.typecheck_and_convert(e)
                if not is_void(target_type):
                    self.validate_type_specifier(target_type)
                if is_void(target_type):
                    # this allows casting any type (including void) to void
                    return syntax.Cast(target_type, e).set_type(target_type)
                if not is_scalar(target_type):
                    self.error('Can only cast to scalar types or void')
                if not is_scalar(e.expr_type):
                    self.error('Cannot cast non-sclar expression to scalar type')
                match (e.expr_type, target_type):
                    case (syntax.Double(), syntax.Pointer()):
                        self.error('Cannot cast doubles to pointers')
                    case (syntax.Pointer(), syntax.Double()):
                        self.error('Cannot cast pointers to doubles')
                expr = syntax.Cast(target_type, e)
                return expr.set_type(target_type)

            case syntax.Dereference(e):
                e = self.typecheck_and_convert(e)
                match e.expr_type:
                    case syntax.Pointer(referenced_type):
                        if is_void(referenced_type):
                            self.error('Cannot dereference a void pointer')
                        expr = syntax.Dereference(e)
                        return expr.set_type(referenced_type)
                    case _:
                        self.error(f'Cannot dereference non-pointer: {e.expr_type}')

            case syntax.AddrOf(e):
                if not is_lvalue(e):
                    self.error(f'Cannot take the address of a non-lvalue: {e}')
                # This intentionally doesn't call typecheck_and_convert because
                # this is the one case where array typed expressions aren't
                # implicitly converted to a pointer.
                e = self.typecheck_expr(e)
                expr = syntax.AddrOf(e)
                pointer_type = syntax.Pointer(e.expr_type)
                return expr.set_type(pointer_type)

            case syntax.Subscript(left, right):
                left = self.typecheck_and_convert(left)
                right = self.typecheck_and_convert(right)
                tl = left.expr_type
                tr = right.expr_type
                if self.is_pointer_to_complete(tl) and self.is_integer(tr):
                    ptr_type = tl
                    right = self.convert_to(right, syntax.Long())
                elif self.is_integer(tl) and self.is_pointer_to_complete(tr):
                    ptr_type = tr
                    left = self.convert_to(left, syntax.Long())
                else:
                    self.error('Subscript must have integer and pointer operand')
                expr = syntax.Subscript(left, right)
                return expr.set_type(ptr_type.referenced)

            case syntax.Dot(expr, member):
                expr = self.typecheck_and_convert(expr)
                expr_type = expr.expr_type
                match expr_type:
                    case syntax.Struct(tag):
                        struct_type = self.types.get(tag)
                        assert(isinstance(struct_type, StructType))
                        struct_member = struct_type.get_member(member)
                        if struct_member is None:
                            self.error(f'structure has no member with this name: {member} struct: {tag}')
                        return syntax.Dot(expr, member).set_type(struct_member.type)
                    case syntax.Union(tag):
                        union_type = self.types.get(tag)
                        assert(isinstance(union_type, UnionType))
                        union_member = union_type.get_member(member)
                        if union_member is None:
                            self.error(f'union has no member with this name: {member} union: {tag}')
                        return syntax.Dot(expr, member).set_type(union_member.type)
                    case _:
                        self.error(f'cannot access member of non-struct type: {member} {expr_type}')

            case syntax.Arrow(expr, member):
                expr = self.typecheck_and_convert(expr)
                expr_type = expr.expr_type
                match expr_type:
                    case syntax.Pointer(syntax.Struct(tag)):
                        struct_type = self.types.get(tag)
                        assert(isinstance(struct_type, StructType))
                        struct_member = struct_type.get_member(member)
                        if struct_member is None:
                            self.error(f'structure has no member with this name: {member} struct: {tag}')
                        return syntax.Arrow(expr, member).set_type(struct_member.type)
                    case syntax.Pointer(syntax.Union(tag)):
                        union_type = self.types.get(tag)
                        assert(isinstance(union_type, UnionType))
                        union_member = union_type.get_member(member)
                        if union_member is None:
                            self.error(f'union has no member with this name: {member} union: {tag}')
                        return syntax.Arrow(expr, member).set_type(union_member.type)
                    case _:
                        self.error(f'invalid type for the -> operator: {expr_type}')

            case syntax.SizeOf(expr):
                # Don't turn arrays into pointers, so this calls typecheck_expr
                # instead of typecheck_and_convert:
                expr = self.typecheck_expr(expr)
                if not self.is_complete(expr.expr_type):
                    self.error('Cannot find the sizeof an incomplete type')
                return syntax.SizeOf(expr).set_type(syntax.ULong())

            case syntax.SizeOfT(type):
                self.validate_type_specifier(type)
                if not self.is_complete(type):
                    self.error('Cannot find the sizeof an incomplete type')
                return syntax.SizeOfT(type).set_type(syntax.ULong())

            case _:
                raise Exception(f'Unhandled type of expression {expr}')

    def typecheck_binary(self, expression: syntax.Binary) -> syntax.Binary:
        l = self.typecheck_and_convert(expression.left)
        r = self.typecheck_and_convert(expression.right)
        op = expression.operator

        if not is_scalar(l.expr_type) or not is_scalar(r.expr_type):
            self.error(f'Cannot apply binary operations to non-scalar types')

        match op:
            case syntax.Equals():
                return self.typecheck_binary_equality(op, l, r)
            case syntax.NotEquals():
                return self.typecheck_binary_equality(op, l, r)
            case syntax.BinaryAdd():
                return self.typecheck_binary_add(l, r)
            case syntax.BinarySubtract():
                return self.typecheck_binary_subtract(l, r)
            case syntax.Less() | syntax.LessEqual() | syntax.Greater() | syntax.GreaterEqual():
                return self.typecheck_binary_comparison(op, l, r)

        common_type = self.get_common_type(l.expr_type, r.expr_type)

        if common_type == syntax.Double():
            non_double_ops = [
                syntax.BinaryRemainder(),
                syntax.BitAnd(),
                syntax.BitOr(),
                syntax.BitXor(),
                syntax.ShiftLeft(),
                syntax.ShiftRight(),
            ]
            if op in non_double_ops:
                self.error(f'Cannot apply operator {op} to doubles')

        if isinstance(common_type, syntax.Pointer):
            self.check_pointer_op(op)

        # && and || always return an int
        if op == syntax.BinaryAnd() or op == syntax.BinaryOr():
            expr = syntax.Binary(op, l, r)
            return expr.set_type(syntax.Int())

        # << and >> always take the type of the left hand side
        if op == syntax.ShiftLeft() or op == syntax.ShiftRight():
            result_type = l.expr_type
            if self.is_character(result_type):
                result_type = syntax.Int()
                l = self.convert_to(l, result_type)
            expr = syntax.Binary(op, l, r)
            return expr.set_type(result_type)

        converted_l = self.convert_to(l, common_type)
        converted_r = self.convert_to(r, common_type)
        expr = syntax.Binary(op, converted_l, converted_r)
        return expr.set_type(common_type)

    def typecheck_binary_comparison(self, op, l, r):
        l_is_pointer = self.is_pointer(l.expr_type)
        r_is_pointer = self.is_pointer(r.expr_type)
        if l_is_pointer != r_is_pointer:
            self.error('cannot compare pointers and non-pointers')
        elif l_is_pointer and is_void_pointer(l.expr_type) != is_void_pointer(r.expr_type):
            self.error('cannot compare void pointers to other pointer types')

        if l_is_pointer:
            common_type = self.get_common_pointer_type(l, r)
        else:
            common_type = self.get_common_type(l.expr_type, r.expr_type)

        converted_l = self.convert_to(l, common_type)
        converted_r = self.convert_to(r, common_type)
        expr = syntax.Binary(op, converted_l, converted_r)

        # E.g. `>=` of two longs returns an int
        return expr.set_type(syntax.Int())

    def typecheck_binary_add(self, l, r):
        op = syntax.BinaryAdd()
        if self.is_arithmetic(l.expr_type) and self.is_arithmetic(r.expr_type):
            common_type = self.get_common_type(l.expr_type, r.expr_type)
            converted_l = self.convert_to(l, common_type)
            converted_r = self.convert_to(r, common_type)
            expr = syntax.Binary(op, converted_l, converted_r)
            return expr.set_type(common_type)
        elif self.is_pointer_to_complete(l.expr_type) and self.is_integer(r.expr_type):
            converted_r = self.convert_to(r, syntax.Long())
            expr = syntax.Binary(op, l, converted_r)
            return expr.set_type(l.expr_type)
        elif self.is_integer(l.expr_type) and self.is_pointer_to_complete(r.expr_type):
            converted_l = self.convert_to(l, syntax.Long())
            expr = syntax.Binary(op, converted_l, r)
            return expr.set_type(r.expr_type)
        else:
            self.error(f'Invalid operands for addition: {l} and {r}')

    def typecheck_binary_subtract(self, l, r):
        op = syntax.BinarySubtract()
        if self.is_arithmetic(l.expr_type) and self.is_arithmetic(r.expr_type):
            common_type = self.get_common_type(l.expr_type, r.expr_type)
            converted_l = self.convert_to(l, common_type)
            converted_r = self.convert_to(r, common_type)
            expr = syntax.Binary(op, converted_l, converted_r)
            return expr.set_type(common_type)
        elif self.is_pointer_to_complete(l.expr_type) and self.is_integer(r.expr_type):
            converted_r = self.convert_to(r, syntax.Long())
            expr = syntax.Binary(op, l, converted_r)
            return expr.set_type(l.expr_type)
        elif self.is_pointer_to_complete(l.expr_type) and l.expr_type == r.expr_type:
            expr = syntax.Binary(op, l, r)
            return expr.set_type(syntax.Long())
        else:
            self.error(f'Invalid operands for subtraction: {l} and {r}')

    def check_pointer_op(self, op):
        non_ponter_ops = [
            syntax.BinaryRemainder(),
            syntax.BinaryDivide(),
            syntax.BinaryMultiply(),
            syntax.ShiftLeft(),
            syntax.ShiftRight(),
            syntax.BitAnd(),
            syntax.BitOr(),
            syntax.BitXor(),
        ]
        if op in non_ponter_ops:
            self.error(f'Cannot apply operator {op} to pointers')

    def typecheck_binary_equality(self, op, l, r):
        ''' equality operators can compare pointers '''
        assert(op == syntax.Equals() or op == syntax.NotEquals())
        l_type = l.expr_type
        r_type = r.expr_type
        if isinstance(l_type, syntax.Pointer) or isinstance(r_type, syntax.Pointer):
            common_type = self.get_common_pointer_type(l, r)
        elif self.is_arithmetic(l_type) and self.is_arithmetic(r_type):
            common_type = self.get_common_type(l_type, r_type)
        else:
            self.error(f'cannot apply {op} to {l_type} and {r_type}')
        l_converted = self.convert_to(l, common_type)
        r_converted = self.convert_to(r, common_type)
        expr = syntax.Binary(op, l_converted, r_converted)
        return expr.set_type(syntax.Int())

    def convert_to(self, expression, target_type):
        assert(expression.expr_type is not None)
        if not is_scalar(expression.expr_type):
            self.error(f'cannot cast from a non-scalar type: {expression.expr_type}')
        if not is_scalar(target_type):
            self.error(f'cannot cast to a non-scalar type: {target_type}')

        if expression.expr_type == target_type:
            return expression
        # Special cases where a cast can be avoided
        match (expression, target_type):
            case (syntax.Constant(syntax.ConstInt(val)), syntax.Long()):
                return syntax.Constant(syntax.ConstLong(val)).set_type(target_type)
            case (syntax.Constant(syntax.ConstUInt(val)), syntax.ULong()):
                return syntax.Constant(syntax.ConstULong(val)).set_type(target_type)
            case (syntax.Constant(syntax.ConstUInt(val)), syntax.Long()):
                return syntax.Constant(syntax.ConstLong(val)).set_type(target_type)
        # Add a cast for other types
        cast_expr = syntax.Cast(target_type, expression)
        cast_expr.set_type(target_type)
        return cast_expr

    def convert_by_assignment(self, expression, target_type):
        expr_type = expression.expr_type
        assert(expr_type is not None)
        void_pointer = syntax.Pointer(syntax.Void())
        if expr_type == target_type:
            return expression
        if self.is_arithmetic(expr_type) and self.is_arithmetic(target_type):
            return self.convert_to(expression, target_type)
        if self.is_null_pointer_constant(expression) and isinstance(target_type, syntax.Pointer):
            return self.convert_to(expression, target_type)
        if target_type == void_pointer and self.is_pointer(expr_type):
            return self.convert_to(expression, target_type)
        if self.is_pointer(target_type) and expr_type == void_pointer:
            return self.convert_to(expression, target_type)
        self.error(f'cannot convert type for assignment {expression} {expr_type} to {target_type}')

    def is_arithmetic(self, t):
        match t:
            case syntax.Char() | syntax.UChar() | syntax.SChar():
                return True
            case syntax.Int() | syntax.UInt():
                return True
            case syntax.Long() | syntax.ULong():
                return True
            case syntax.Double():
                return True
            case _:
                return False

    def is_integer(self, t):
        match t:
            case syntax.Char() | syntax.UChar() | syntax.SChar():
                return True
            case syntax.Int() | syntax.UInt():
                return True
            case syntax.Long() | syntax.ULong():
                return True
            case _:
                return False

    def is_array(self, t):
        match t:
            case syntax.Array():
                return True
            case _:
                return False

    def is_pointer(self, t):
        match t:
            case syntax.Pointer():
                return True
            case _:
                return False

    def is_character(self, t):
        match t:
            case syntax.Char() | syntax.UChar() | syntax.SChar():
                return True
            case _:
                return False

    def get_common_type(self, t1, t2):
        if self.is_character(t1):
            t1 = syntax.Int()
        if self.is_character(t2):
            t2 = syntax.Int()

        if t1 == t2:
            return t1
        if syntax.Double() in [t1, t2]:
            return syntax.Double()
        if self.get_size(t1) == self.get_size(t2):
            if typeconversion.is_signed(t1):
                return t2
            else:
                return t1
        if self.get_size(t1) > self.get_size(t2):
            return t1
        else:
            return t2
        raise Exception(f'Unhandled combination of types {t1} and {t2}')

    def get_common_pointer_type(self, e1: syntax.Expression, e2: syntax.Expression) -> syntax.Type:
        t1 = e1.expr_type
        t2 = e2.expr_type
        void_pointer = syntax.Pointer(syntax.Void())
        if t1 == t2:
            return t1
        elif self.is_null_pointer_constant(e1):
            return t2
        elif self.is_null_pointer_constant(e2):
            return t1
        elif t1 == void_pointer and self.is_pointer(t2):
            return void_pointer
        elif t2 == void_pointer and self.is_pointer(t1):
            return void_pointer
        else:
            self.error(f'expressions have incompatible pointer types: {e1} and {e2}')

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
                    expr = self.typecheck_and_convert(expr)
                return syntax.InitExp(expr)
            case None:
                return None
            case _:
                raise Exception(f'invalid type for ForInit {init}')

    def validate_non_void_type(self, type: syntax.Type):
        match type:
            case syntax.Void():
                self.error(f'Illegal array of incomplete type {elem_type}')
            case syntax.Array(elem_type, size):
                if is_void(elem_type):
                    self.error(f'Illegal array of incomplete type {elem_type}')
                self.validate_non_void_type(elem_type)
            case syntax.Pointer(referenced_type):
                if not is_void(referenced_type):
                    self.validate_non_void_type(referenced_type)
            case syntax.Func(params, ret):
                for param_t in params:
                    self.validate_non_void_type(param_t)
                if not is_void(ret):
                    self.validate_non_void_type(ret)
            case _:
                pass

    def validate_type_specifier(self, type: syntax.Type):
        match type:
            case syntax.Array(elem_type, size):
                if not self.is_complete(elem_type):
                    self.error(f'Illegal array of incomplete type {elem_type}')
                self.validate_type_specifier(elem_type)
            case syntax.Pointer(referenced_type):
                if not is_void(referenced_type) and not is_struct_or_union(referenced_type):
                    self.validate_type_specifier(referenced_type)
            case syntax.Func(params, ret):
                for param_t in params:
                    self.validate_type_specifier(param_t)
                if not is_void(ret):
                    self.validate_type_specifier(ret)
            case _:
                if not self.is_complete(type):
                    self.error(f'use of incomplete type {type}')

    def is_complete(self, type: syntax.Type) -> bool:
        match type:
            case syntax.Void():
                return False
            case syntax.Struct(tag):
                return tag in self.types
            case syntax.Union(tag):
                return tag in self.types
            case _:
                return True

    def is_pointer_to_complete(self, type: syntax.Type) -> bool:
        match type:
            case syntax.Pointer(t):
                return self.is_complete(t)
            case _:
                return False


def is_lvalue(expr: syntax.Expression) -> bool:
    match expr:
        case syntax.String():
            # Allow taking a pointer to a string with &
            return True
        case syntax.Variable():
            return True
        case syntax.Dereference():
            return True
        case syntax.Subscript():
            return True
        case syntax.Arrow():
            return True
        case syntax.Dot(left, _):
            return is_lvalue(left)
        case _:
            return False


def is_scalar(type: syntax.Type) -> bool:
    match type:
        case syntax.Void():
            return False
        case syntax.Array():
            return False
        case syntax.Func():
            return False
        case syntax.Struct():
            return False
        case syntax.Union():
            return False
        case _:
            return True


def is_struct_or_union(type: syntax.Type) -> bool:
    match type:
        case syntax.Struct():
            return True
        case syntax.Union():
            return True
        case _:
            return False


def is_void(type: syntax.Type) -> bool:
    match type:
        case syntax.Void():
            return True
        case _:
            return False


def is_void_pointer(type: syntax.Type) -> bool:
    match type:
        case syntax.Pointer(syntax.Void()):
            return True
        case _:
            return False


def round_up(size: int, alignment: int) -> int:
    ''' return the next smallest size that is aligned to alignment '''
    remainder = size % alignment
    if remainder == 0:
        return size
    padding = alignment - remainder
    return size + padding
