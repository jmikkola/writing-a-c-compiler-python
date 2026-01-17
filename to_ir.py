import syntax
import tacky
import symbol


def to_ir(syntax: syntax.Program, symbols: dict) -> tacky.Program:
    tt = ToTacky(syntax, symbols)
    return tt.convert()


class ToTacky:
    def __init__(self, syntax, symbols):
        self.syntax = syntax
        self.symbols = symbols
        self.n_temp_vars = 0
        self.n_labels = 0
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
        else:
            raise Exception(f'unhandled type of constant {const_type}')

    def new_label(self, name):
        name = f'_{name}_{self.n_labels}'
        self.n_labels += 1
        return name

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
                            top_level.append(tacky.StaticVariable(name, is_global, var_type, init))
                        case symbol.Initial(value):
                            top_level.append(tacky.StaticVariable(name, is_global, var_type, init))
                        case symbol.NoInitializer():
                            pass
                        case _:
                            assert(False)
                case symbol.FuncAttr():
                    pass
                case symbol.LocalAttr():
                    pass
                case _:
                    assert(False)

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
        instructions, result = self.convert_expression(init)
        instructions.append(tacky.Copy(result, tacky.Identifier(name)))
        return instructions

    def convert_do_while(self, stmt: syntax.DoWhile) -> list:
        loop_start = self.new_label('do_while_start')
        loop_label = stmt.loop_label
        continue_label = f'continue_{loop_label}'
        break_label = f'break_{loop_label}'

        instructions = [tacky.Label(loop_start)]
        instructions += self.convert_instructions(stmt.body)
        instructions.append(tacky.Label(continue_label))
        test_instructions, test_val = self.convert_expression(stmt.test)
        instructions += test_instructions
        instructions.append(tacky.JumpIfNotZero(test_val, loop_start))
        instructions.append(tacky.Label(break_label))
        return instructions

    def convert_while(self, stmt: syntax.While) -> list:
        loop_label = stmt.loop_label
        continue_label = f'continue_{loop_label}'
        break_label = f'break_{loop_label}'

        instructions = [tacky.Label(continue_label)]
        test_instructions, test_val = self.convert_expression(stmt.test)
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
            test_instrs, test_val = self.convert_expression(stmt.condition)
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
        instructions, val = self.convert_expression(stmt.condition)
        # Handle jumping to the different case statements
        for case_value in stmt.case_values:
            if case_value == 'default':
                continue
            result_var = self.make_tacky_variable(stmt.condition.expr_type)
            const_val = tacky.Constant(self.convert_constant(case_value))
            instructions += [
                tacky.Binary(tacky.BinarySubtract(), val, const_val, result_var),
                tacky.JumpIfZero(result_var, f'switch_{switch_label}_case_{case_value.value}'),
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
        instructions, val = self.convert_expression(stmt.condition)
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
        instructions, val = self.convert_expression(stmt.expr)
        return instructions + [tacky.Return(val)]

    def convert_expression(self, expr: syntax.Expression) -> (list, tacky.Value):
        ''' returns (instructions, result value) '''
        match expr:
            case syntax.Constant(value):
                return ([], tacky.Constant(self.convert_constant(value)))

            case syntax.Cast(_):
                return self.convert_cast(expr)

            case syntax.Variable(name):
                return ([], tacky.Identifier(name))

            case syntax.Assignment(syntax.Variable(name), rhs, None):
                instructions, result = self.convert_expression(rhs)
                instructions += [tacky.Copy(result, tacky.Identifier(name))]
                return (instructions, result)

            case syntax.Assignment(syntax.Variable(name), rhs, op):
                # This logic only works because the assign target has no side effects
                # (e.g. it isn't `a[i++]`) so it can safely be duplicated
                updated_expression = syntax.Assignment(
                    syntax.Variable(name),
                    syntax.Binary(op, syntax.Variable(name), rhs),
                    None
                )
                return self.convert_expression(updated_expression)

            case syntax.Unary(operator, inner):
                match operator:
                    case syntax.UnaryIncrement() | syntax.UnaryDecrement():
                        assert(isinstance(inner,  syntax.Variable))
                        op = self.convert_modifying_op(operator)
                        result_var = self.make_tacky_variable(expr.expr_type)
                        instructions = [
                            tacky.Binary(
                                operator=op,
                                left=tacky.Identifier(inner.name),
                                right=self.make_constant_of_type(1, expr.expr_type),
                                dst=tacky.Identifier(inner.name)
                            ),
                            tacky.Copy(tacky.Identifier(inner.name), result_var)
                        ]
                        return (instructions, result_var)
                    case _:
                        instructions, val = self.convert_expression(inner)
                        op = self.convert_unary_op(operator)
                        result_var = self.make_tacky_variable(expr.expr_type)
                        instruction = tacky.Unary(unary_operator=op, src=val, dst=result_var)
                        return (instructions + [instruction], result_var)

            case syntax.Postfix(expr, operator):
                assert(isinstance(expr,  syntax.Variable))
                op = self.convert_modifying_op(operator)
                result_var = self.make_tacky_variable(expr.expr_type)
                instructions = [
                    tacky.Copy(tacky.Identifier(expr.name), result_var),
                    tacky.Binary(
                        operator=op,
                        left=tacky.Identifier(expr.name),
                        right=self.make_constant_of_type(1, expr.expr_type),
                        dst=tacky.Identifier(expr.name)
                    ),
                ]
                return (instructions, result_var)

            case syntax.Binary(syntax.BinaryAnd(), left, right):
                false_label = self.new_label('and_false')
                end_label = self.new_label('and_end')
                instructions_left, val_left = self.convert_expression(left)
                instructions_right, val_right = self.convert_expression(right)
                result_var = self.make_tacky_variable(expr.expr_type)
                instructions = instructions_left + [tacky.JumpIfZero(val_left, false_label)]
                instructions += instructions_right + [tacky.JumpIfZero(val_right, false_label)]
                instructions += [
                    tacky.Copy(tacky.Constant(tacky.ConstInt(1)), result_var),
                    tacky.Jump(end_label),
                    tacky.Label(false_label),
                    tacky.Copy(tacky.Constant(tacky.ConstInt(0)), result_var),
                    tacky.Label(end_label),
                ]
                return (instructions, result_var)

            case syntax.Binary(syntax.BinaryOr(), left, right):
                true_label = self.new_label('or_true')
                end_label = self.new_label('or_end')
                instructions_left, val_left = self.convert_expression(left)
                instructions_right, val_right = self.convert_expression(right)
                result_var = self.make_tacky_variable(expr.expr_type)
                instructions = instructions_left + [tacky.JumpIfNotZero(val_left, true_label)]
                instructions += instructions_right + [tacky.JumpIfNotZero(val_right, true_label)]
                instructions += [
                    tacky.Copy(tacky.Constant(tacky.ConstInt(0)), result_var),
                    tacky.Jump(end_label),
                    tacky.Label(true_label),
                    tacky.Copy(tacky.Constant(tacky.ConstInt(1)), result_var),
                    tacky.Label(end_label),
                ]
                return (instructions, result_var)

            case syntax.Binary(operator, left, right):
                instructions_left, val_left = self.convert_expression(left)
                instructions_right, val_right = self.convert_expression(right)
                op = self.convert_binary_op(operator)
                result_var = self.make_tacky_variable(expr.expr_type)
                instruction = tacky.Binary(operator=op, left=val_left, right=val_right, dst=result_var)
                instructions = instructions_left + instructions_right + [instruction]
                return (instructions, result_var)

            case syntax.Conditional(_, _, _):
                return self.convert_conditional(expr)

            case syntax.Call(_, _):
                return self.convert_call(expr)

            case _:
                raise Exception(f'unhandled expression type, {expr}')

    def convert_constant(self, constant: syntax.Const) -> tacky.Const:
        match constant:
            case syntax.ConstInt(x):
                return tacky.ConstInt(x)
            case syntax.ConstLong(x):
                return tacky.ConstLong(x)
            case _:
                raise Exception(f'unhandled constant type {constant}')

    def convert_cast(self, expr: syntax.Cast):
        inner = expr.expr
        target_type = expr.target_type

        instructions, val = self.convert_expression(inner)
        if target_type == inner.expr_type:
            # No cast needed
            return (instructions, val)

        dst = self.make_tacky_variable(target_type)
        if target_type == syntax.Long():
            instructions.append(tacky.SignExtend(val, dst))
        else:
            instructions.append(tacky.Truncate(val, dst))

        return (instructions, dst)

    def convert_conditional(self, expr: syntax.Conditional):
        false_label = self.new_label('cond_false')
        end_label = self.new_label('cond_end')
        result_var = self.make_tacky_variable(expr.expr_type)

        instructions, val = self.convert_expression(expr.condition)
        instructions.append(tacky.JumpIfZero(val, false_label))

        t_instructions, t_val = self.convert_expression(expr.t)
        instructions += t_instructions
        instructions.append(tacky.Copy(t_val, result_var))
        instructions.append(tacky.Jump(end_label))

        instructions.append(tacky.Label(false_label))
        e_instructions, e_val = self.convert_expression(expr.e)
        instructions += e_instructions
        instructions.append(tacky.Copy(e_val, result_var))
        instructions.append(tacky.Label(end_label))

        return (instructions, result_var)

    def convert_call(self, expr: syntax.Call):
        function = expr.function
        instructions = []
        arg_vals = []

        for arg in expr.arguments:
            i, val = self.convert_expression(arg)
            instructions.extend(i)
            arg_vals.append(val)

        dst = self.make_tacky_variable(expr.expr_type)
        instructions.append(tacky.Call(function, arg_vals, dst))
        return (instructions, dst)

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
