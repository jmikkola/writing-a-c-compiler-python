from __future__ import annotations
from dataclasses import dataclass
import typing

import labels
import syntax
import tacky
import symbol
import typeconversion


def to_ir(syntax: syntax.Program, symbols: dict, types: dict, label_gen: labels.Labels) -> tacky.Program:
    tt = ToTacky(syntax, symbols, types, label_gen)
    return tt.convert()


class ToTacky:
    def __init__(self, syntax, symbols, types, label_gen):
        self.syntax = syntax
        self.symbols = symbols
        self.types = types
        self.label_gen = label_gen
        self.n_temp_vars = 0
        # To be set by convert_function
        self.user_labels = None

    def new_temp_var(self):
        name = f'tmp.{self.n_temp_vars}'
        self.n_temp_vars += 1
        return tacky.Identifier(name)

    def make_tacky_variable(self, var_type: syntax.Type):
        assert(var_type is not None)
        var = self.new_temp_var()
        self.symbols[var.name] = symbol.Symbol(var_type, symbol.LocalAttr())
        return var

    def make_constant_of_type(self, constant, const_type: syntax.Type):
        if const_type == syntax.Int():
            return tacky.Constant(tacky.ConstInt(constant))
        elif const_type == syntax.Long():
            return tacky.Constant(tacky.ConstLong(constant))
        elif const_type == syntax.UInt():
            return tacky.Constant(tacky.ConstUInt(constant))
        elif const_type == syntax.ULong():
            return tacky.Constant(tacky.ConstULong(constant))
        elif const_type == syntax.Double():
            return tacky.Constant(tacky.ConstDouble(float(constant)))
        elif const_type == syntax.Char() or const_type == syntax.SChar():
            return tacky.Constant(tacky.ConstChar(constant))
        elif const_type == syntax.UChar():
            # A signed char value probably works
            return tacky.Constant(tacky.ConstChar(constant))
        else:
            raise Exception(f'unhandled type of constant {const_type}')

    def new_label(self, name):
        return self.label_gen.new_label(name)

    def rename_user_label(self, label):
        if label not in self.user_labels:
            self.user_labels[label] = self.new_label(label)
        return self.user_labels[label]

    def convert(self):
        top_level = []
        for decl in self.syntax.declarations:
            match decl:
                case syntax.FuncDeclaration():
                    if decl.body is not None:
                        top_level.append(self.convert_function(decl))
                case syntax.VarDeclaration():
                    pass
                case syntax.StructDeclaration():
                    pass
                case _:
                    raise Exception(f'unhandled {decl}')

        top_level += self.convert_symbols()

        return tacky.Program(top_level)

    def convert_symbols(self):
        top_level = []

        for (name, entry) in self.symbols.items():
            var_type = entry.type
            match entry.attrs:
                case symbol.StaticAttr(init, is_global):
                    match init:
                        case symbol.Tentative():
                            n_bytes = typeconversion.type_size(var_type)
                            values = [symbol.ZeroInit(n_bytes)]
                            top_level.append(tacky.StaticVariable(name, is_global, var_type, values))
                        case symbol.Initial(values):
                            top_level.append(tacky.StaticVariable(name, is_global, var_type, values))
                        case symbol.NoInitializer():
                            pass
                        case _:
                            raise Exception(f'unhandled init type {init}')
                case symbol.ConstantAttr(init):
                    match init:
                        case symbol.Initial([value]):
                            top_level.append(tacky.StaticConstant(name, var_type, value))
                        case _:
                            raise Exception(f'invalid initializer for a constant: {init}')
                case symbol.FuncAttr():
                    pass
                case symbol.LocalAttr():
                    pass
                case _:
                    raise Exception(f'unhandled attribute type {entry.attrs}')

        return top_level

    def convert_function(self, function: syntax.FuncDeclaration) -> tacky.Function:
        self.user_labels = {}
        instructions = self.convert_block(function.body)
        instructions.append(tacky.Return(tacky.Constant(tacky.ConstInt(0))))
        sym = self.symbols[function.name]
        is_global = sym.attrs.is_global
        return tacky.Function(
            name=function.name,
            is_global=is_global,
            params=function.params,
            body=instructions
        )

    def convert_block(self, block: syntax.Block):
        instructions = []
        for block_item in block.block_items:
            instructions.extend(self.convert_instructions(block_item))
        return instructions

    def convert_instructions(self, body: syntax.BlockItem) -> list:
        match body:
            case syntax.Return(_):
                return self.convert_return(body)
            case syntax.ExprStmt(expr):
                instructions, _ = self.convert_expression(expr)
                return instructions
            case syntax.NullStatement():
                return []
            case syntax.IfStatement(_, _, _):
                return self.convert_if(body)
            case syntax.Goto(label):
                label = self.rename_user_label(label)
                return [tacky.Jump(label)]
            case syntax.LabeledStmt(label, stmt):
                label = self.rename_user_label(label)
                instructions = self.convert_instructions(stmt)
                return [tacky.Label(label)] + instructions
            case syntax.VarDeclaration(name, init):
                return self.convert_declaration(name, init)
            case syntax.FuncDeclaration(_, _, _):
                return []
            case syntax.Compound(block):
                return self.convert_block(block)
            case syntax.Continue(loop_label):
                return [tacky.Jump(f'continue_{loop_label}')]
            case syntax.Break(loop_label):
                return [tacky.Jump(f'break_{loop_label}')]
            case syntax.DoWhile(_, _, _):
                return self.convert_do_while(body)
            case syntax.While(_, _, _):
                return self.convert_while(body)
            case syntax.For(_, _, _, _, _):
                return self.convert_for(body)
            case syntax.Switch(_, _, _):
                return self.convert_switch(body)
            case syntax.Case(_, _, _):
                return self.convert_case(body)
            case syntax.Default(_, _):
                return self.convert_default(body)
            case _:
                raise Exception(f'unhandled statement type, {body}')

    def convert_declaration(self, name, init):
        if init is None:
            return []
        sym = self.symbols[name]
        # Static variables aren't initialized inside the function
        if isinstance(sym.attrs, symbol.StaticAttr):
            return []
        return self.emit_initializer(name, init)

    def emit_initializer(self, name: str, init: syntax.Initializer, offset=None):
        match init:
            case syntax.SingleInit(syntax.String(bytes)) if isinstance(init.expr_type, syntax.Array):
                return self.emit_string_initializer(name, offset, bytes, init.expr_type)

            case syntax.SingleInit(expr):
                instructions, result = self.emit_tacky_and_convert(expr)
                if offset is not None:
                    copy = tacky.CopyToOffset(result, tacky.Identifier(name), offset)
                else:
                    copy = tacky.Copy(result, tacky.Identifier(name))
                instructions.append(copy)
                return instructions

            case syntax.CompoundInit(initializers):
                if offset is None:
                    offset = 0
                scale = typeconversion.type_size(init.expr_type.element)
                instructions = []
                for initializer in initializers:
                    instructions.extend(self.emit_initializer(name, initializer, offset))
                    offset += scale
                return instructions

            case _:
                raise Exception(f'unhandled type of initializer: {init}')

    def is_char(self, t):
        match t:
            case syntax.Char() | syntax.UChar() | syntax.SChar():
                return True
            case _:
                return False

    def emit_string_initializer(self, name, offset, bytes, var_type):
        assert(isinstance(var_type, syntax.Array))
        assert(self.is_char(var_type.element))

        array_size = var_type.size
        if array_size > len(bytes):
            # Pad with zeros (the first of which acts as the null terminator)
            bytes = bytes + [0] * (array_size - len(bytes))

        if offset is None:
            offset = 0
        instructions = []
        i = 0
        identifier = tacky.Identifier(name)
        # Take 4 bytes at a time
        while i + 3 < len(bytes):
            packed = self.pack_bytes(bytes[i:i+4])
            constant = tacky.Constant(tacky.ConstInt(packed))
            instructions.append(tacky.CopyToOffset(constant, identifier, offset))
            i += 4
            offset += 4
        while i < len(bytes):
            byte = bytes[i]
            constant = tacky.Constant(tacky.ConstChar(byte))
            instructions.append(tacky.CopyToOffset(constant, identifier, offset))
            i += 1
            offset += 1
        return instructions

    def pack_bytes(self, bytes) -> int:
        ''' pack the bytes into an integer in little endian order '''
        result = 0
        multiple = 1
        for b in bytes:
            result += b * multiple
            multiple *= 256
        return result

    def convert_do_while(self, stmt: syntax.DoWhile) -> list:
        loop_start = self.new_label('do_while_start')
        loop_label = stmt.loop_label
        continue_label = f'continue_{loop_label}'
        break_label = f'break_{loop_label}'

        instructions = [tacky.Label(loop_start)]
        instructions += self.convert_instructions(stmt.body)
        instructions.append(tacky.Label(continue_label))
        test_instructions, test_val = self.emit_tacky_and_convert(stmt.test)
        instructions += test_instructions
        instructions.append(tacky.JumpIfNotZero(test_val, loop_start))
        instructions.append(tacky.Label(break_label))
        return instructions

    def convert_while(self, stmt: syntax.While) -> list:
        loop_label = stmt.loop_label
        continue_label = f'continue_{loop_label}'
        break_label = f'break_{loop_label}'

        instructions = [tacky.Label(continue_label)]
        test_instructions, test_val = self.emit_tacky_and_convert(stmt.test)
        instructions += test_instructions
        instructions.append(tacky.JumpIfZero(test_val, break_label))
        instructions += self.convert_instructions(stmt.body)
        instructions.append(tacky.Jump(continue_label))
        instructions.append(tacky.Label(break_label))
        return instructions

    def convert_for(self, stmt: syntax.For) -> list:
        loop_label = stmt.loop_label
        continue_label = f'continue_{loop_label}'
        break_label = f'break_{loop_label}'
        start_label = self.new_label('for_start')

        instructions = self.convert_for_init(stmt.init)
        instructions.append(tacky.Label(start_label))
        if stmt.condition:
            test_instrs, test_val = self.emit_tacky_and_convert(stmt.condition)
            instructions += test_instrs
            instructions.append(tacky.JumpIfZero(test_val, break_label))
        instructions += self.convert_instructions(stmt.body)
        instructions.append(tacky.Label(continue_label))
        if stmt.post:
            post_instrs, _ = self.convert_expression(stmt.post)
            instructions += post_instrs
        instructions.append(tacky.Jump(start_label))
        instructions.append(tacky.Label(break_label))
        return instructions

    def convert_for_init(self, init: syntax.ForInit) -> list:
        match init:
            case syntax.InitDecl(decl):
                return self.convert_instructions(decl)
            case syntax.InitExp(None):
                return []
            case syntax.InitExp(expr):
                instructions, _ = self.convert_expression(expr)
                return instructions
            case _:
                raise Exception(f'unhandled for init {init}')

    def convert_switch(self, stmt: syntax.Switch) -> list:
        switch_label = stmt.switch_label
        break_label = f'break_{switch_label}'
        # Convert the condition
        instructions, val = self.emit_tacky_and_convert(stmt.condition)
        # Handle jumping to the different case statements
        for case_value in stmt.case_values:
            if case_value == 'default':
                continue
            result_var = self.make_tacky_variable(stmt.condition.expr_type)
            const_val = tacky.Constant(self.convert_constant(case_value))
            value = case_value.value
            if value < 0:
                value = 'neg' + str(-value)
            instructions += [
                tacky.Binary(tacky.BinarySubtract(), val, const_val, result_var),
                tacky.JumpIfZero(result_var, f'switch_{switch_label}_case_{value}'),
            ]
        # Handle no case statements matching
        if 'default' in stmt.case_values:
            instructions.append(tacky.Jump(f'switch_{switch_label}_default'))
        else:
            instructions.append(tacky.Jump(break_label))
        # Add the code inside the switch
        instructions += self.convert_instructions(stmt.body)
        instructions.append(tacky.Label(break_label))
        return instructions

    def convert_case(self, stmt: syntax.Case) -> list:
        switch_label = stmt.switch_label
        value = stmt.value.const.value
        if value < 0:
            value = 'neg' + str(-value)
        instructions = [tacky.Label(f'switch_{switch_label}_case_{value}')]
        if stmt.stmt:
            instructions += self.convert_instructions(stmt.stmt)
        return instructions

    def convert_default(self, stmt: syntax.Default) -> list:
        switch_label = stmt.switch_label
        instructions = [tacky.Label(f'switch_{switch_label}_default')]
        if stmt.stmt:
            instructions += self.convert_instructions(stmt.stmt)
        return instructions

    def convert_if(self, stmt: syntax.IfStatement) -> list:
        after_then = self.new_label('if_false')
        after_else = self.new_label('if_end')
        instructions, val = self.emit_tacky_and_convert(stmt.condition)
        instructions.append(tacky.JumpIfZero(val, after_then))
        instructions += self.convert_instructions(stmt.t)
        if stmt.e:
            instructions.append(tacky.Jump(after_else))
        instructions.append(tacky.Label(after_then))
        if stmt.e:
            instructions += self.convert_instructions(stmt.e)
            instructions.append(tacky.Label(after_else))
        return instructions

    def convert_return(self, stmt: syntax.Return) -> list:
        if stmt.expr is None:
            return [tacky.Return(None)]
        instructions, val = self.emit_tacky_and_convert(stmt.expr)
        return instructions + [tacky.Return(val)]

    def emit_tacky_and_convert(self, expr: syntax.Expression) -> typing.Tuple[list, tacky.Value]:
        instructions, result = self.convert_expression(expr)
        match result:
            case PlainOperand(val):
                return instructions, val
            case DereferencedPointer(ptr):
                dst = self.make_tacky_variable(expr.expr_type)
                instructions.append(tacky.Load(ptr, dst))
                return instructions, dst
            case _:
                raise Exception(f'Unhandled type of result: {result}')

    def convert_expression(self, expr: syntax.Expression) -> typing.Tuple[list, ExpResult]:
        ''' returns (instructions, result value) '''
        match expr:
            case syntax.Constant(value):
                const = tacky.Constant(self.convert_constant(value))
                return ([], PlainOperand(const))

            case syntax.Cast(_):
                return self.convert_cast(expr)

            case syntax.Variable(name):
                return ([], PlainOperand(tacky.Identifier(name)))

            case syntax.Assignment(_, _, None):
                return self.convert_simple_assignment(expr)

            case syntax.Assignment(_, _, op):
                return self.convert_operator_assignment(expr)

            case syntax.Unary(operator, inner):
                match operator:
                    case syntax.UnaryIncrement() | syntax.UnaryDecrement():
                        instructions, inner_result = self.convert_expression(inner)

                        op = self.convert_modifying_op(operator)
                        result_var = self.make_tacky_variable(expr.expr_type)

                        match inner_result:
                            case PlainOperand(val):
                                match expr.expr_type:
                                    case syntax.Pointer(inner):
                                        stride = typeconversion.type_size(inner)
                                        right = self.make_constant_of_type(stride, syntax.ULong())
                                    case _:
                                        right = self.make_constant_of_type(1, expr.expr_type)
                                # E.g. ++x
                                instructions += [
                                    tacky.Binary(
                                        operator=op,
                                        left=val,
                                        right=right,
                                        dst=val
                                    ),
                                    tacky.Copy(val, result_var)
                                ]
                                return (instructions, PlainOperand(result_var))
                            case DereferencedPointer(ptr):
                                # E.g. ++(*x)
                                # Get the value by dereferencing the pointer
                                current_value = self.make_tacky_variable(expr.expr_type)
                                instructions += [
                                    tacky.Load(ptr, current_value),
                                    tacky.Binary(
                                        operator=op,
                                        left=current_value,
                                        right=self.make_constant_of_type(1, expr.expr_type),
                                        dst=result_var
                                    ),
                                    tacky.Store(result_var, ptr)
                                ]
                                return (instructions, PlainOperand(result_var))
                    case _:
                        instructions, val = self.emit_tacky_and_convert(inner)
                        op = self.convert_unary_op(operator)
                        result_var = self.make_tacky_variable(expr.expr_type)
                        instruction = tacky.Unary(unary_operator=op, src=val, dst=result_var)
                        return (instructions + [instruction], PlainOperand(result_var))

            case syntax.Postfix(expr, operator):
                instructions, inner_result = self.convert_expression(expr)
                op = self.convert_modifying_op(operator)
                output_var = self.make_tacky_variable(expr.expr_type)

                match inner_result:
                    case PlainOperand(val):
                        match expr.expr_type:
                            case syntax.Pointer(inner):
                                stride = typeconversion.type_size(inner)
                                right = self.make_constant_of_type(stride, syntax.ULong())
                            case _:
                                right = self.make_constant_of_type(1, expr.expr_type)
                        # e.g. x++
                        instructions += [
                            tacky.Copy(val, output_var),
                            tacky.Binary(
                                operator=op,
                                left=output_var,
                                right=right,
                                dst=val
                            ),
                        ]
                        return (instructions, PlainOperand(output_var))
                    case DereferencedPointer(ptr):
                        # e.g. (*x)++
                        result_var = self.make_tacky_variable(expr.expr_type)
                        instructions += [
                            tacky.Load(ptr, output_var),
                            tacky.Binary(
                                operator=op,
                                left=output_var,
                                right=self.make_constant_of_type(1, expr.expr_type),
                                dst=result_var,
                            ),
                            tacky.Store(result_var, ptr)
                        ]
                        return (instructions, PlainOperand(output_var))

            case syntax.Binary():
                return self.convert_binary(expr)

            case syntax.Sequence():
                return self.convert_sequence(expr)

            case syntax.Conditional():
                return self.convert_conditional(expr)

            case syntax.Call():
                return self.convert_call(expr)

            case syntax.Dereference(inner):
                instructions, result = self.emit_tacky_and_convert(inner)
                return (instructions, DereferencedPointer(result))

            case syntax.AddrOf(inner):
                instructions, result = self.convert_expression(inner)
                match result:
                    case PlainOperand(obj):
                        dst = self.make_tacky_variable(expr.expr_type)
                        instructions.append(tacky.GetAddress(obj, dst))
                        return (instructions, PlainOperand(dst))
                    case DereferencedPointer(ptr):
                        return (instructions, PlainOperand(ptr))

            case syntax.Subscript(left, right):
                left_instructions, left_result = self.emit_tacky_and_convert(left)
                right_instructions, right_result = self.emit_tacky_and_convert(right)
                instructions = left_instructions + right_instructions
                result = self.make_tacky_variable(syntax.Pointer(expr.expr_type))
                if is_pointer(left.expr_type):
                    scale = typeconversion.type_size(left.expr_type.referenced)
                    instruction = tacky.AddPtr(left_result, right_result, scale, result)
                else:
                    assert(is_pointer(right.expr_type))
                    scale = typeconversion.type_size(right.expr_type.referenced)
                    instruction = tacky.AddPtr(right_result, left_result, scale, result)
                instructions.append(instruction)
                return (instructions, DereferencedPointer(result))

            case syntax.String(bytes):
                const_name = self.new_temp_var()
                var_type = syntax.Array(syntax.Char(), len(bytes) + 1)
                string_value = symbol.Initial([symbol.StringInit(bytes, True)])
                attr = symbol.ConstantAttr(init=string_value)
                self.symbols[const_name.name] = symbol.Symbol(var_type, attr)
                return ([], PlainOperand(const_name))

            case syntax.SizeOf(expr):
                t = expr.expr_type
                result = typeconversion.type_size(t)
                return ([], PlainOperand(tacky.Constant(tacky.ConstULong(result))))

            case syntax.SizeOfT(type):
                result = typeconversion.type_size(type)
                return ([], PlainOperand(tacky.Constant(tacky.ConstULong(result))))

            case _:
                raise Exception(f'unhandled expression type, {expr}')

    def convert_simple_assignment(self, expr: syntax.Assignment):
        assert(expr.op is None)
        l_instructions, lval_expr = self.convert_expression(expr.lhs)
        r_instructions, rval = self.emit_tacky_and_convert(expr.rhs)
        instructions = l_instructions + r_instructions
        match lval_expr:
            case PlainOperand(obj):
                instructions.append(tacky.Copy(rval, obj))
                return (instructions, lval_expr)
            case DereferencedPointer(ptr):
                instructions.append(tacky.Store(rval, ptr))
                return (instructions, PlainOperand(rval))

    def convert_operator_assignment(self, expr: syntax.Assignment):
        assert(expr.op is not None)

        instructions, lval_expr = self.convert_expression(expr.lhs)
        match lval_expr:
            case PlainOperand(val):
                # This should mean that the lhs was just a variable
                left_expr = expr.lhs

                b_expression = syntax.Binary(expr.op, left_expr, expr.rhs).set_type(expr.expr_type)
                b_instructions, bval = self.emit_tacky_and_convert(b_expression)
                instructions += b_instructions

                instructions.append(tacky.Copy(bval, val))
                return (instructions, PlainOperand(val))

            case DereferencedPointer(ptr):
                # The lhs dereferences a pointer, so the left hand side of the binary operator
                # be loaded from that pointer
                left_temp = self.make_tacky_variable(expr.expr_type)
                instructions.append(tacky.Load(ptr, left_temp))
                left_expr = syntax.Variable(left_temp.name).set_type(expr.expr_type)

                b_expression = syntax.Binary(expr.op, left_expr, expr.rhs).set_type(expr.expr_type)
                b_instructions, bval = self.emit_tacky_and_convert(b_expression)
                instructions += b_instructions

                instructions.append(tacky.Store(bval, ptr))
                return (instructions, PlainOperand(bval))

    def convert_binary(self, expr):
        match expr.operator:
            case syntax.BinaryAnd():
                false_label = self.new_label('and_false')
                end_label = self.new_label('and_end')
                instructions_left, val_left = self.emit_tacky_and_convert(expr.left)
                instructions_right, val_right = self.emit_tacky_and_convert(expr.right)
                result_var = self.make_tacky_variable(syntax.Int())
                instructions = instructions_left + [tacky.JumpIfZero(val_left, false_label)]
                instructions += instructions_right + [tacky.JumpIfZero(val_right, false_label)]
                instructions += [
                    tacky.Copy(tacky.Constant(tacky.ConstInt(1)), result_var),
                    tacky.Jump(end_label),
                    tacky.Label(false_label),
                    tacky.Copy(tacky.Constant(tacky.ConstInt(0)), result_var),
                    tacky.Label(end_label),
                ]
                return (instructions, PlainOperand(result_var))

            case syntax.BinaryOr():
                true_label = self.new_label('or_true')
                end_label = self.new_label('or_end')
                instructions_left, val_left = self.emit_tacky_and_convert(expr.left)
                instructions_right, val_right = self.emit_tacky_and_convert(expr.right)
                result_var = self.make_tacky_variable(syntax.Int())
                instructions = instructions_left + [tacky.JumpIfNotZero(val_left, true_label)]
                instructions += instructions_right + [tacky.JumpIfNotZero(val_right, true_label)]
                instructions += [
                    tacky.Copy(tacky.Constant(tacky.ConstInt(0)), result_var),
                    tacky.Jump(end_label),
                    tacky.Label(true_label),
                    tacky.Copy(tacky.Constant(tacky.ConstInt(1)), result_var),
                    tacky.Label(end_label),
                ]
                return (instructions, PlainOperand(result_var))

            case syntax.BinaryAdd():
                instructions_left, val_left = self.emit_tacky_and_convert(expr.left)
                instructions_right, val_right = self.emit_tacky_and_convert(expr.right)
                op = self.convert_binary_op(expr.operator)
                result_var = self.make_tacky_variable(expr.expr_type)

                left_type = expr.left.expr_type
                right_type = expr.right.expr_type
                if is_pointer(left_type) and is_integer(right_type):
                    scale = typeconversion.type_size(left_type.referenced)
                    instruction = tacky.AddPtr(val_left, val_right, scale, result_var)
                elif is_integer(left_type) and is_pointer(right_type):
                    scale = typeconversion.type_size(right_type.referenced)
                    instruction = tacky.AddPtr(val_right, val_left, scale, result_var)
                else:
                    instruction = tacky.Binary(op, val_left, val_right, result_var)
                instructions = instructions_left + instructions_right
                instructions.append(instruction)
                return (instructions, PlainOperand(result_var))

            case syntax.BinarySubtract():
                instructions_left, val_left = self.emit_tacky_and_convert(expr.left)
                instructions_right, val_right = self.emit_tacky_and_convert(expr.right)
                instructions = instructions_left + instructions_right
                op = self.convert_binary_op(expr.operator)
                result_var = self.make_tacky_variable(expr.expr_type)

                left_type = expr.left.expr_type
                right_type = expr.right.expr_type
                if is_pointer(left_type) and is_integer(right_type):
                    negated_result_var = self.make_tacky_variable(right_type)
                    negate = tacky.Unary(tacky.UnaryNegate(), val_right, negated_result_var)
                    instructions.append(negate)

                    scale = typeconversion.type_size(left_type.referenced)
                    add_ptr = tacky.AddPtr(val_left, negated_result_var, scale, result_var)
                    instructions.append(add_ptr)
                elif is_pointer(left_type) and is_pointer(right_type):
                    scale = typeconversion.type_size(right_type.referenced)
                    unscaled_result_var = self.make_tacky_variable(expr.expr_type)
                    sub = tacky.Binary(op, val_left, val_right, unscaled_result_var)
                    instructions.append(sub)
                    div = tacky.Binary(
                        tacky.BinaryDivide(),
                        unscaled_result_var,
                        tacky.Constant(tacky.ConstInt(scale)),
                        result_var
                    )
                    instructions.append(div)

                else:
                    binary = tacky.Binary(operator=op, left=val_left, right=val_right, dst=result_var)
                    instructions.append(binary)

                return (instructions, PlainOperand(result_var))

            case _:
                instructions_left, val_left = self.emit_tacky_and_convert(expr.left)
                instructions_right, val_right = self.emit_tacky_and_convert(expr.right)
                op = self.convert_binary_op(expr.operator)
                result_var = self.make_tacky_variable(expr.expr_type)
                instruction = tacky.Binary(operator=op, left=val_left, right=val_right, dst=result_var)
                instructions = instructions_left + instructions_right + [instruction]
                return (instructions, PlainOperand(result_var))

    def convert_constant(self, constant: syntax.Const) -> tacky.Const:
        match constant:
            case syntax.ConstInt(x):
                return tacky.ConstInt(x)
            case syntax.ConstLong(x):
                return tacky.ConstLong(x)
            case syntax.ConstUInt(x):
                return tacky.ConstUInt(x)
            case syntax.ConstULong(x):
                return tacky.ConstULong(x)
            case syntax.ConstDouble(x):
                return tacky.ConstDouble(x)
            case _:
                raise Exception(f'unhandled constant type {constant}')

    def convert_cast(self, expr: syntax.Cast):
        inner = expr.expr
        inner_type = inner.expr_type
        target_type = expr.target_type

        instructions, val = self.emit_tacky_and_convert(inner)
        if target_type == inner_type or target_type == syntax.Void():
            # No cast needed
            return (instructions, PlainOperand(val))

        dst = self.make_tacky_variable(target_type)

        if inner_type == syntax.Double():
            if typeconversion.is_signed(target_type):
                instructions.append(tacky.DoubleToInt(val, dst))
            else:
                instructions.append(tacky.DoubleToUInt(val, dst))
        elif target_type == syntax.Double():
            if typeconversion.is_signed(inner_type):
                instructions.append(tacky.IntToDouble(val, dst))
            else:
                instructions.append(tacky.UIntToDouble(val, dst))
        elif typeconversion.type_size(target_type) == typeconversion.type_size(inner_type):
            instructions.append(tacky.Copy(val, dst))
        elif typeconversion.type_size(target_type) < typeconversion.type_size(inner_type):
            instructions.append(tacky.Truncate(val, dst))
        elif typeconversion.is_signed(inner_type):
            instructions.append(tacky.SignExtend(val, dst))
        else:
            instructions.append(tacky.ZeroExtend(val, dst))

        return (instructions, PlainOperand(dst))

    def convert_sequence(self, expr: syntax.Sequence):
        # Evaluate the first experssion and throw away the result
        instructions_first, _ = self.convert_expression(expr.first)
        # Evaluate the second expression and keep the value
        instructions_second, val = self.convert_expression(expr.second)
        return (instructions_first + instructions_second, val)

    def convert_conditional(self, expr: syntax.Conditional):
        false_label = self.new_label('cond_false')
        end_label = self.new_label('cond_end')
        is_void_result = expr.t.expr_type == syntax.Void()
        if is_void_result:
            result_var = 'UNUSED'
        else:
            result_var = self.make_tacky_variable(expr.expr_type)

        instructions, val = self.emit_tacky_and_convert(expr.condition)
        instructions.append(tacky.JumpIfZero(val, false_label))

        t_instructions, t_val = self.emit_tacky_and_convert(expr.t)
        instructions += t_instructions
        if not is_void_result:
            instructions.append(tacky.Copy(t_val, result_var))
        instructions.append(tacky.Jump(end_label))

        instructions.append(tacky.Label(false_label))
        e_instructions, e_val = self.emit_tacky_and_convert(expr.e)
        instructions += e_instructions
        if not is_void_result:
            instructions.append(tacky.Copy(e_val, result_var))
        instructions.append(tacky.Label(end_label))

        return (instructions, PlainOperand(result_var))

    def convert_call(self, expr: syntax.Call):
        function = expr.function
        instructions = []
        arg_vals = []

        for arg in expr.arguments:
            i, val = self.emit_tacky_and_convert(arg)
            instructions.extend(i)
            arg_vals.append(val)

        dst = None
        if expr.expr_type != syntax.Void():
            dst = self.make_tacky_variable(expr.expr_type)
        instructions.append(tacky.Call(function, arg_vals, dst))
        return (instructions, PlainOperand(dst))

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
            case syntax.Less():
                return tacky.Less()
            case syntax.LessEqual():
                return tacky.LessEqual()
            case syntax.Greater():
                return tacky.Greater()
            case syntax.GreaterEqual():
                return tacky.GreaterEqual()
            case syntax.Equals():
                return tacky.Equals()
            case syntax.NotEquals():
                return tacky.NotEquals()
            case _:
                raise Exception(f'unhandled binary operator {op}')

    def convert_unary_op(self, op: syntax.UnaryOp) -> tacky.UnaryOp:
        match op:
            case syntax.UnaryNegate():
                return tacky.UnaryNegate()
            case syntax.UnaryInvert():
                return tacky.UnaryInvert()
            case syntax.UnaryNot():
                return tacky.UnaryNot()
            case _:
                raise Exception(f'unhandled unary operator {op}')

    def convert_modifying_op(self, op: syntax.UnaryOp) -> tacky.BinaryOp:
        match op:
            case syntax.UnaryIncrement():
                return tacky.BinaryAdd()
            case syntax.UnaryDecrement():
                return tacky.BinarySubtract()
            case _:
                raise Exception(f'not a modifying operator {op}')


def is_pointer(t: syntax.Type):
    match t:
        case syntax.Pointer():
            return True
        case _:
            return False


def is_integer(t: syntax.Type):
    match t:
        case syntax.Int() | syntax.UInt():
            return True
        case syntax.Long() | syntax.ULong():
            return True
        case _:
            return False


@dataclass
class PlainOperand:
    val: tacky.Value


@dataclass
class DereferencedPointer:
    val: tacky.Value


@dataclass
class SubObject:
    base: str
    offset: int


ExpResult = typing.Union[PlainOperand, DereferencedPointer, SubObject]
