import assembly
import tacky


def gen(tacky: tacky.Program) -> assembly.Program:
    cg = Codegen(tacky)
    return cg.generate()


class Codegen:
    def __init__(self, tacky):
        self.tacky = tacky

    def generate(self):
        function = self.gen_function(self.tacky.function_definition)
        return assembly.Program(function_definition=function)

    def gen_function(self, function: tacky.Function) -> assembly.Function:
        instructions = self.gen_instructions(function.body)
        instructions, stack_size = self.replace_pseudo_registers(instructions)
        instructions = [assembly.AllocateStack(stack_size)] + instructions
        instructions = self.fix_invalid_instructions(instructions)
        return assembly.Function(name=function.name, instructions=instructions)

    def replace_pseudo_registers(self, instructions):
        pseudo_registers = {}
        updated_instructions = []
        for instr in instructions:
            match instr:
                case assembly.Ret() | assembly.Cdq():
                    pass
                case assembly.Jmp() | assembly.JmpCC() | assembly.Label():
                    pass
                case assembly.Mov(src, dst):
                    src = self.convert_pseudo_register(pseudo_registers, src)
                    dst = self.convert_pseudo_register(pseudo_registers, dst)
                    instr = assembly.Mov(src, dst)
                case assembly.AllocateStack(_):
                    pass
                case assembly.Unary(unary_operator, operand):
                    operand = self.convert_pseudo_register(pseudo_registers, operand)
                    instr = assembly.Unary(unary_operator, operand)
                case assembly.Binary(binary_operator, left, right):
                    left = self.convert_pseudo_register(pseudo_registers, left)
                    right = self.convert_pseudo_register(pseudo_registers, right)
                    instr = assembly.Binary(binary_operator, left, right)
                case assembly.Idiv(operand):
                    operand = self.convert_pseudo_register(pseudo_registers, operand)
                    instr = assembly.Idiv(operand)
                case assembly.Cmp(left, right):
                    left = self.convert_pseudo_register(pseudo_registers, left)
                    right = self.convert_pseudo_register(pseudo_registers, right)
                    instr = assembly.Cmp(left, right)
                case assembly.SetCC(cond_code, operand):
                    operand = self.convert_pseudo_register(pseudo_registers, operand)
                    instr = assembly.SetCC(cond_code, operand)
                case _:
                    raise Exception(f'unhandled instruction type {instr}')
            updated_instructions.append(instr)
        return (updated_instructions, 4 * len(pseudo_registers))

    def convert_pseudo_register(self, pseudo_registers: dict, operand: assembly.Operand) -> assembly.Operand:
        if not isinstance(operand, assembly.Pseudo):
            return operand
        name = operand.name
        if name not in pseudo_registers:
            pseudo_registers[name] = -4 * (1 + len(pseudo_registers))
        offset = pseudo_registers[name]
        return assembly.Stack(offset)

    def fix_invalid_instructions(self, instructions):
        ''' Fix invalid instructions.

        E.g. mov instructions that use a memory address as both
        the source and destination
        '''
        r10 = assembly.Register('R10')
        r11 = assembly.Register('R11')

        updated_instructions = []
        for instr in instructions:
            match instr:
                case assembly.Mov(assembly.Stack(_) as src, assembly.Stack(_) as dst):
                    updated_instructions.extend([
                        assembly.Mov(src, r10),
                        assembly.Mov(r10, dst),
                    ])

                case assembly.Cmp(assembly.Stack(_) as left, assembly.Stack(_) as right):
                    updated_instructions.extend([
                        assembly.Mov(left, r10),
                        assembly.Cmp(r10, right)
                    ])

                case assembly.Cmp(left, assembly.Immediate(_) as right):
                    updated_instructions.extend([
                        assembly.Mov(right, r11),
                        assembly.Cmp(left, r11),
                    ])

                case assembly.Idiv(assembly.Immediate(_) as imm):
                    updated_instructions.extend([
                        assembly.Mov(imm, r10),
                        assembly.Idiv(r10),
                    ])

                case assembly.Binary(assembly.Mult() as op, src, assembly.Stack(_) as dst):
                    # It's important that Mult is handled differently from other
                    # binary operations because it can't have a memory address
                    # in the destination
                    updated_instructions.extend([
                        assembly.Mov(dst, r11),
                        assembly.Binary(op, src, r11),
                        assembly.Mov(r11, dst),
                    ])

                case assembly.Binary(op, assembly.Stack(_) as src, assembly.Stack(_) as dst):
                    updated_instructions.extend([
                        assembly.Mov(src, r10),
                        assembly.Binary(op, r10, dst),
                    ])

                case _:
                    updated_instructions.append(instr)
        return updated_instructions

    def gen_instructions(self, body: list) -> list:
        result = []
        for instruction in body:
            result += self.gen_instruction(instruction)
        return result

    def gen_instruction(self, instr: tacky.Instruction) -> list:
        ''' returns a list of assembly instructions '''
        assert(isinstance(instr, tacky.Instruction))
        match instr:
            case tacky.Return(_):
                return self.gen_return(instr)
            case tacky.Unary(_, _, _):
                return self.gen_unary(instr)
            case tacky.Binary(_, _, _, _):
                return self.gen_binary(instr)
            case tacky.Copy(src, dst):
                return [
                    assembly.Mov(self.convert_operand(src), self.convert_operand(dst)),
                ]
            case tacky.Jump(target):
                return [assembly.Jmp(target)]
            case tacky.JumpIfZero(cond, target):
                return [
                    assembly.Cmp(assembly.Immediate(0), self.convert_operand(cond)),
                    assembly.JmpCC('E', target),
                ]
            case tacky.JumpIfNotZero(cond, target):
                return [
                    assembly.Cmp(assembly.Immediate(0), self.convert_operand(cond)),
                    assembly.JmpCC('NE', target),
                ]
            case tacky.Label(name):
                return [assembly.Label(name)]
            case _:
                raise Exception(f'unhandled instruction type, {instr}')

    def gen_return(self, instr: tacky.Return) -> list:
        src = self.convert_operand(instr.val)
        return [
            assembly.Mov(src=src, dst=assembly.Register('AX')),
            assembly.Ret(),
        ]

    def gen_unary(self, instr: tacky.Unary) -> list:
        src = self.convert_operand(instr.src)
        dst = self.convert_operand(instr.dst)

        if isinstance(instr.unary_operator, tacky.UnaryNot):
            return [
                assembly.Cmp(assembly.Immediate(0), src),
                # Zero the destination because the 'set' instruction only
                # updates the lowest 8 bits.
                assembly.Mov(assembly.Immediate(0), dst),
                assembly.SetCC('E', dst),
            ]

        op = self.convert_unary_operator(instr.unary_operator)
        return [
            assembly.Mov(src, dst),
            assembly.Unary(op, dst),
        ]

    def convert_unary_operator(self, op: tacky.UnaryOp) -> assembly.UnaryOperator:
        match op:
            case tacky.UnaryNegate():
                return assembly.Neg()
            case tacky.UnaryInvert():
                return assembly.Not()
            case _:
                raise Exception(f'unhandled unary op type {op}')

    def gen_binary(self, instr: tacky.Binary) -> list:
        left = self.convert_operand(instr.left)
        right = self.convert_operand(instr.right)
        dst = self.convert_operand(instr.dst)

        match instr.operator:
            case tacky.BinaryAdd() | tacky.BinarySubtract() | tacky.BinaryMultiply():
                op = self.convert_binary_operator(instr.operator)
                return [
                    assembly.Mov(left, dst),
                    assembly.Binary(op, right, dst),
                ]
            case tacky.BitOr() | tacky.BitXor() | tacky.BitAnd():
                op = self.convert_binary_operator(instr.operator)
                return [
                    assembly.Mov(left, dst),
                    assembly.Binary(op, right, dst),
                ]
            case tacky.ShiftLeft() | tacky.ShiftRight():
                op = self.convert_binary_operator(instr.operator)
                return [
                    assembly.Mov(left, dst),
                    assembly.Mov(right, assembly.Register('CX')),
                    assembly.Binary(op, assembly.Register('CX'), dst),
                ]
            case tacky.BinaryDivide():
                return [
                    assembly.Mov(left, assembly.Register('AX')),
                    assembly.Cdq(),
                    assembly.Idiv(right),
                    assembly.Mov(assembly.Register('AX'), dst),
                ]
            case tacky.BinaryRemainder():
                return [
                    assembly.Mov(left, assembly.Register('AX')),
                    assembly.Cdq(),
                    assembly.Idiv(right),
                    assembly.Mov(assembly.Register('DX'), dst),
                ]
            case tacky.Less() | tacky.LessEqual() | tacky.Equals() | \
                 tacky.NotEquals() | tacky.Greater() | tacky.GreaterEqual():
                comparison = self.convert_comparison(instr.operator)
                return [
                    assembly.Cmp(right, left),
                    assembly.Mov(assembly.Immediate(0), dst),
                    assembly.SetCC(comparison, dst),
                ]
            case _:
                raise Exception(f'unhandled binary expression op {instr.operator}')

    def convert_comparison(self, op: tacky.BinaryOp) -> str:
        match op:
            case tacky.Less():
                return 'L'
            case tacky.LessEqual():
                return 'LE'
            case tacky.Equals():
                return 'E'
            case tacky.NotEquals():
                return 'NE'
            case tacky.GreaterEqual():
                return 'GE'
            case tacky.Greater():
                return 'G'
            case _:
                raise Exception(f'invalid op to convert to a comparison {op}')

    def convert_binary_operator(self, op: tacky.BinaryOp) -> assembly.BinaryOperator:
        match op:
            case tacky.BinaryAdd():
                return assembly.Add()
            case tacky.BinarySubtract():
                return assembly.Sub()
            case tacky.BinaryMultiply():
                return assembly.Mult()
            case tacky.BitOr():
                return assembly.BitOr()
            case tacky.BitAnd():
                return assembly.BitAnd()
            case tacky.BitXor():
                return assembly.BitXor()
            case tacky.ShiftLeft():
                return assembly.ShiftLeft()
            case tacky.ShiftRight():
                return assembly.ShiftRight()
            case _:
                raise Exception(f'invalid op to convert to assembly binary op {op}')

    def convert_operand(self, value: tacky.Value) -> assembly.Operand:
        match value:
            case tacky.Constant(value):
                return assembly.Immediate(value)
            case tacky.Identifier(name):
                return assembly.Pseudo(name)
            case _:
                raise Exception(f'unhandled operand type {value}')
