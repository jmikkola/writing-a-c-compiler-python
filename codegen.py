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

                case assembly.Idiv(assembly.Immediate(_) as imm):
                    updated_instructions.extend([
                        assembly.Mov(imm, r10),
                        assembly.Idiv(r10),
                    ])

                case assembly.Binary(
                        assembly.Add() | assembly.Sub() as op,
                        assembly.Stack(_) as src,
                        assembly.Stack(_) as dst):
                    updated_instructions.extend([
                        assembly.Mov(src, r10),
                        assembly.Binary(op, r10, dst),
                    ])

                case assembly.Binary(assembly.Mult() as op, src, assembly.Stack(_) as dst):
                    updated_instructions.extend([
                        assembly.Mov(dst, r11),
                        assembly.Binary(op, src, r11),
                        assembly.Mov(r11, dst),
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
        match instr:
            case tacky.Return(_):
                return self.gen_return(instr)
            case tacky.Unary(_, _, _):
                return self.gen_unary(instr)
            case tacky.Binary(_, _, _, _):
                return self.gen_binary(instr)
            case _:
                raise Exception(f'unhandled statement type, {instr}')

    def gen_return(self, instr: tacky.Return) -> list:
        src = self.convert_operand(instr.val)
        return [
            assembly.Mov(src=src, dst=assembly.Register('AX')),
            assembly.Ret(),
        ]

    def gen_unary(self, instr: tacky.Unary) -> list:
        op = self.convert_unary_operator(instr.unary_operator)
        src = self.convert_operand(instr.src)
        dst = self.convert_operand(instr.dst)
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
            case _:
                raise Exception(f'unhandled binary expression op {instr.operator}')

    def convert_binary_operator(self, op: tacky.BinaryOp) -> assembly.BinaryOperator:
        match op:
            case tacky.BinaryAdd():
                return assembly.Add()
            case tacky.BinarySubtract():
                return assembly.Sub()
            case tacky.BinaryMultiply():
                return assembly.Mult()
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
