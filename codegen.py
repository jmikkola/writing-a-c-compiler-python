import assembly
import tacky
import validator


def gen(tacky: tacky.Program, symbols: dict) -> assembly.Program:
    cg = Codegen(tacky, symbols)
    return cg.generate()


class Codegen:
    def __init__(self, tacky, symbols):
        self.tacky = tacky
        self.symbols = symbols
        self.arg_registers = ['DI', 'SI', 'DX', 'CX', 'R8', 'R9']

    def generate(self):
        top_level = [
            self.gen_top_level(d)
            for d in self.tacky.top_level
        ]
        return assembly.Program(top_level)

    def gen_top_level(self, top_level):
        match top_level:
            case tacky.Function():
                return self.gen_function(top_level)
            case tacky.StaticVariable():
                return self.gen_static_var(top_level)
            case _:
                assert(False)

    def gen_static_var(self, var: tacky.StaticVariable) -> assembly.StaticVariable:
        return assembly.StaticVariable(
            name=var.name,
            is_global=var.is_global,
            init=var.init,
        )

    def gen_function(self, function: tacky.Function) -> assembly.Function:
        # Generate the basic assembly
        instructions = self.save_arguments(function.params)
        instructions += self.gen_instructions(function.body)

        # Replace pseudo registers with stack locations
        instructions, stack_size = self.replace_pseudo_registers(instructions)
        if stack_size % 16 != 0:
            stack_size += 16 - (stack_size % 16)
        instructions = [assembly.AllocateStack(stack_size)] + instructions

        # Fix instructions that are now invalid
        instructions = self.fix_invalid_instructions(instructions)

        return assembly.Function(
            name=function.name,
            is_global=function.is_global,
            instructions=instructions
        )

    def save_arguments(self, params):
        ''' For simplicity, copy all parameters to the current stack frame '''
        instructions = []

        # Handle arguments that are passed in registers
        for (param, reg) in zip(params, self.arg_registers):
            instructions.append(assembly.Mov(assembly.Register(reg), assembly.Pseudo(param)))

        # Handle arguments that are passed on the stack
        stack_offset = 16
        stack_params = params[6:]
        for param in stack_params:
            instructions.append(assembly.Mov(assembly.Stack(stack_offset), assembly.Pseudo(param)))
            stack_offset += 8

        return instructions

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
                case assembly.AllocateStack(_) | assembly.DeallocateStack(_):
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
                case assembly.Call(_):
                    pass
                case assembly.Push(operand):
                    operand = self.convert_pseudo_register(pseudo_registers, operand)
                    instr = assembly.Push(operand)
                case _:
                    raise Exception(f'unhandled instruction type {instr}')
            updated_instructions.append(instr)
        return (updated_instructions, 4 * len(pseudo_registers))

    def convert_pseudo_register(self, pseudo_registers: dict, operand: assembly.Operand) -> assembly.Operand:
        if not isinstance(operand, assembly.Pseudo):
            return operand
        name = operand.name
        if name not in pseudo_registers:
            if name in self.symbols and isinstance(self.symbols[name].attrs, validator.StaticAttr):
                return assembly.Data(name)
            else:
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
                case assembly.Mov(src, dst) if is_mem(src) and is_mem(dst):
                    updated_instructions.extend([
                        assembly.Mov(src, r10),
                        assembly.Mov(r10, dst),
                    ])

                case assembly.Cmp(left, right) if is_mem(left) and is_mem(right):
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

                case assembly.Binary(assembly.Mult() as op, src, dst) if is_mem(dst):
                    # It's important that Mult is handled differently from other
                    # binary operations because it can't have a memory address
                    # in the destination
                    updated_instructions.extend([
                        assembly.Mov(dst, r11),
                        assembly.Binary(op, src, r11),
                        assembly.Mov(r11, dst),
                    ])

                case assembly.Binary(op, src, dst) if is_mem(src) and is_mem(dst):
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
            case tacky.Call(_, _, _):
                return self.gen_call(instr)
            case _:
                raise Exception(f'unhandled instruction type, {instr}')

    def gen_call(self, instr: tacky.Call) -> list:
        register_args, stack_args = (instr.arg_vals[:6], instr.arg_vals[6:])
        stack_padding = 8 * (len(stack_args) % 2)

        instructions = []

        # Add stack padding so the alignment comes out right
        if stack_padding > 0:
            instructions.append(assembly.AllocateStack(stack_padding))

        # Pass the first 6 arguments in registers
        for (arg, register) in zip(register_args, self.arg_registers):
            assembly_arg = self.convert_operand(arg)
            instructions.append(assembly.Mov(assembly_arg, assembly.Register(register)))

        # Pass the remaining arguments on the stack
        for arg in stack_args[::-1]:
            assembly_arg = self.convert_operand(arg)
            if isinstance(assembly_arg, assembly.Register) or isinstance(assembly_arg, assembly.Immediate):
                instructions.append(assembly.Push(assembly_arg))
            else:
                instructions.append(assembly.Mov(assembly_arg, assembly.Register('AX')))
                instructions.append(assembly.Push(assembly.Register('AX')))

        # The actual function call
        instructions.append(assembly.Call(instr.func_name))

        # Clean up the stack
        bytes_to_remove = 8 * len(stack_args) + stack_padding
        if bytes_to_remove > 0:
            instructions.append(assembly.DeallocateStack(bytes_to_remove))

        # Move the result to the correct destination
        assembly_dst = self.convert_operand(instr.dst)
        instructions.append(assembly.Mov(assembly.Register('AX'), assembly_dst))

        return instructions

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


def is_mem(operand: assembly.Operand):
    match operand:
        case assembly.Stack():
            return True
        case assembly.Data():
            return True
        case _:
            return False
