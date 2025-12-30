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
        instructions = self.fix_mov(instructions)
        return assembly.Function(name=function.name, instructions=instructions)

    def replace_pseudo_registers(self, instructions):
        pseudo_registers = {}
        updated_instructions = []
        for instr in instructions:
            match instr:
                case assembly.Ret():
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

    def fix_mov(self, instructions):
        ''' Fix mov instructions that use a memory address as both the source and destination '''
        updated_instructions = []
        for instr in instructions:
            match instr:
                case assembly.Mov(assembly.Stack(src_offset), assembly.Stack(dst_offset)):
                    updated_instructions.extend([
                        assembly.Mov(assembly.Stack(src_offset), assembly.Register('R10')),
                        assembly.Mov(assembly.Register('R10'),   assembly.Stack(dst_offset)),
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
            case _:
                raise Exception(f'unhandled statement type, {body}')

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

    def convert_operand(self, value: tacky.Value) -> assembly.Operand:
        match value:
            case tacky.Constant(value):
                return assembly.Immediate(value)
            case tacky.Identifier(name):
                return assembly.Pseudo(name)
            case _:
                raise Exception(f'unhandled operand type {value}')
