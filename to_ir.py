import syntax
import tacky


def to_ir(syntax: syntax.Program) -> tacky.Program:
    tt = ToTacky(syntax)
    return tt.convert()


class ToTacky:
    def __init__(self, syntax):
        self.syntax = syntax
        self.n_temp_vars = 0
        self.n_labels = 0

    def new_temp_var(self):
        name = f'tmp.{self.n_temp_vars}'
        self.n_temp_vars += 1
        return tacky.Identifier(name)

    def new_label(self, name):
        name = f'_{name}_{self.n_labels}'
        self.n_labels += 1
        return name

    def convert(self):
        function = self.convert_function(self.syntax.function_definition)
        return tacky.Program(function_definition=function)

    def convert_function(self, function: syntax.Function) -> tacky.Function:
        instructions = []
        for block_item in function.body:
            instructions.extend(self.convert_instructions(block_item))
        instructions.append(tacky.Return(tacky.Constant(0)))
        return tacky.Function(name=function.name, body=instructions)

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
                return [tacky.Jump(label)]
            case syntax.LabeledStmt(label, stmt):
                instructions = self.convert_instructions(stmt)
                return [tacky.Label(label)] + instructions
            case syntax.Declaration(name, init):
                return self.convert_declaration(name, init)
            case _:
                raise Exception(f'unhandled statement type, {body}')

    def convert_declaration(self, name, init):
        if init is None:
            return []
        instructions, result = self.convert_expression(init)
        instructions.append(tacky.Copy(result, tacky.Identifier(name)))
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
                return ([], tacky.Constant(value))

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

            case syntax.Unary(operator, expr=inner):
                match operator:
                    case syntax.UnaryIncrement() | syntax.UnaryDecrement():
                        assert(isinstance(inner,  syntax.Variable))
                        op = self.convert_modifying_op(operator)
                        result_var = self.new_temp_var()
                        instructions = [
                            tacky.Binary(
                                operator=op,
                                left=tacky.Identifier(inner.name),
                                right=tacky.Constant(1),
                                dst=tacky.Identifier(inner.name)
                            ),
                            tacky.Copy(tacky.Identifier(inner.name), result_var)
                        ]
                        return (instructions, result_var)
                    case _:
                        instructions, val = self.convert_expression(inner)
                        op = self.convert_unary_op(operator)
                        result_var = self.new_temp_var()
                        instruction = tacky.Unary(unary_operator=op, src=val, dst=result_var)
                        return (instructions + [instruction], result_var)

            case syntax.Postfix(expr, operator):
                assert(isinstance(expr,  syntax.Variable))
                op = self.convert_modifying_op(operator)
                result_var = self.new_temp_var()
                instructions = [
                    tacky.Copy(tacky.Identifier(expr.name), result_var),
                    tacky.Binary(
                        operator=op,
                        left=tacky.Identifier(expr.name),
                        right=tacky.Constant(1),
                        dst=tacky.Identifier(expr.name)
                    ),
                ]
                return (instructions, result_var)

            case syntax.Binary(syntax.BinaryAnd(), left, right):
                false_label = self.new_label('and_false')
                end_label = self.new_label('and_end')
                instructions_left, val_left = self.convert_expression(left)
                instructions_right, val_right = self.convert_expression(right)
                result_var = self.new_temp_var()
                instructions = instructions_left + [tacky.JumpIfZero(val_left, false_label)]
                instructions += instructions_right + [tacky.JumpIfZero(val_right, false_label)]
                instructions += [
                    tacky.Copy(tacky.Constant(1), result_var),
                    tacky.Jump(end_label),
                    tacky.Label(false_label),
                    tacky.Copy(tacky.Constant(0), result_var),
                    tacky.Label(end_label),
                ]
                return (instructions, result_var)

            case syntax.Binary(syntax.BinaryOr(), left, right):
                true_label = self.new_label('or_true')
                end_label = self.new_label('or_end')
                instructions_left, val_left = self.convert_expression(left)
                instructions_right, val_right = self.convert_expression(right)
                result_var = self.new_temp_var()
                instructions = instructions_left + [tacky.JumpIfNotZero(val_left, true_label)]
                instructions += instructions_right + [tacky.JumpIfNotZero(val_right, true_label)]
                instructions += [
                    tacky.Copy(tacky.Constant(0), result_var),
                    tacky.Jump(end_label),
                    tacky.Label(true_label),
                    tacky.Copy(tacky.Constant(1), result_var),
                    tacky.Label(end_label),
                ]
                return (instructions, result_var)

            case syntax.Binary(operator, left, right):
                instructions_left, val_left = self.convert_expression(left)
                instructions_right, val_right = self.convert_expression(right)
                op = self.convert_binary_op(operator)
                result_var = self.new_temp_var()
                instruction = tacky.Binary(operator=op, left=val_left, right=val_right, dst=result_var)
                instructions = instructions_left + instructions_right + [instruction]
                return (instructions, result_var)

            case syntax.Conditional(_, _, _):
                return self.convert_conditional(expr)

            case _:
                raise Exception(f'unhandled expression type, {expr}')

    def convert_conditional(self, expr: syntax.Conditional):
        false_label = self.new_label('cond_false')
        end_label = self.new_label('cond_end')
        result_var = self.new_temp_var()

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
