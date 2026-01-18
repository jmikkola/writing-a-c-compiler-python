import assembly
import tacky
import symbol
import syntax


r10 = assembly.Register('R10')
r11 = assembly.Register('R11')
longword = assembly.AssemblyType.Longword
quadword = assembly.AssemblyType.Quadword


def gen(tacky: tacky.Program, symbols: dict) -> (assembly.Program, dict):
    cg = Codegen(tacky, symbols)
    return cg.generate(), cg.asm_symbols


class Codegen:
    def __init__(self, tacky, symbols):
        self.tacky = tacky
        self.symbols = symbols
        self.asm_symbols = self.convert_symbols()
        self.arg_registers = ['DI', 'SI', 'DX', 'CX', 'R8', 'R9']

    def generate(self):
        top_level = [
            self.gen_top_level(d)
            for d in self.tacky.top_level
        ]
        return assembly.Program(top_level)

    def convert_symbols(self):
        return {
            name: self.convert_symbol(sym)
            for (name, sym) in self.symbols.items()
        }

    def convert_symbol(self, sym: symbol.Symbol) -> assembly.AsmSymbol:
        attrs = sym.attrs
        match attrs:
            case symbol.FuncAttr(is_defined, is_global):
                return assembly.FunEntry(is_defined)
            case symbol.StaticAttr(init, is_global):
                a_type = self.sym_type_to_a_type(sym.type)
                return assembly.ObjEntry(a_type, True)
            case symbol.LocalAttr():
                a_type = self.sym_type_to_a_type(sym.type)
                return assembly.ObjEntry(a_type, False)
            case _:
                assert(False)

    def gen_top_level(self, top_level):
        match top_level:
            case tacky.Function():
                return self.gen_function(top_level)
            case tacky.StaticVariable():
                return self.gen_static_var(top_level)
            case _:
                assert(False)

    def gen_static_var(self, var: tacky.StaticVariable) -> assembly.StaticVariable:
        alignment = self.alignment_of(var.var_type)
        return assembly.StaticVariable(
            name=var.name,
            is_global=var.is_global,
            alignment=alignment,
            init=var.init,
        )

    def alignment_of(self, var_type):
        match var_type:
            case syntax.Int():
                return 4
            case syntax.Long():
                return 8
            case _:
                raise Exception(f'Unexpected type to find alignment of {var_type}')

    def gen_function(self, function: tacky.Function) -> assembly.Function:
        # Generate the basic assembly
        instructions = self.save_arguments(function.params)
        instructions += self.gen_instructions(function.body)

        # Replace pseudo registers with stack locations
        instructions, stack_size = self.replace_pseudo_registers(instructions)
        if stack_size % 16 != 0:
            stack_size += 16 - (stack_size % 16)

        allocate = assembly.Binary(
            assembly.Sub(),
            quadword,
            assembly.Immediate(stack_size),
            assembly.Register('SP')
        )
        instructions = [allocate] + instructions

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
            a_type = self.a_type_of(tacky.Identifier(param))
            instructions.append(assembly.Mov(a_type, assembly.Register(reg), assembly.Pseudo(param)))

        # Handle arguments that are passed on the stack
        stack_offset = 16
        stack_params = params[6:]
        for param in stack_params:
            a_type = self.a_type_of(tacky.Identifier(param))
            instructions.append(assembly.Mov(a_type, assembly.Stack(stack_offset), assembly.Pseudo(param)))
            stack_offset += 8

        return instructions

    def replace_pseudo_registers(self, instructions):
        stack_map = StackMap(self.asm_symbols)

        updated_instructions = []
        for instr in instructions:
            match instr:
                case assembly.Ret() | assembly.Cdq():
                    pass
                case assembly.Jmp() | assembly.JmpCC() | assembly.Label():
                    pass
                case assembly.Mov(assembly_type, src, dst):
                    src = stack_map.convert_pseudo_register(src)
                    dst = stack_map.convert_pseudo_register(dst)
                    instr = assembly.Mov(assembly_type, src, dst)
                case assembly.Movsx(src, dst):
                    src = stack_map.convert_pseudo_register(src)
                    dst = stack_map.convert_pseudo_register(dst)
                    instr = assembly.Movsx(src, dst)
                case assembly.Unary(unary_operator, assembly_type, operand):
                    operand = stack_map.convert_pseudo_register(operand)
                    instr = assembly.Unary(unary_operator, assembly_type, operand)
                case assembly.Binary(binary_operator, assembly_type, left, right):
                    left = stack_map.convert_pseudo_register(left)
                    right = stack_map.convert_pseudo_register(right)
                    instr = assembly.Binary(binary_operator, assembly_type, left, right)
                case assembly.Idiv(assembly_type, operand):
                    operand = stack_map.convert_pseudo_register(operand)
                    instr = assembly.Idiv(assembly_type, operand)
                case assembly.Cmp(assembly_type, left, right):
                    left = stack_map.convert_pseudo_register(left)
                    right = stack_map.convert_pseudo_register(right)
                    instr = assembly.Cmp(assembly_type, left, right)
                case assembly.SetCC(cond_code, operand):
                    operand = stack_map.convert_pseudo_register(operand)
                    instr = assembly.SetCC(cond_code, operand)
                case assembly.Call(_):
                    pass
                case assembly.Push(operand):
                    operand = stack_map.convert_pseudo_register(operand)
                    instr = assembly.Push(operand)
                case _:
                    raise Exception(f'unhandled instruction type {instr}')
            updated_instructions.append(instr)
        return (updated_instructions, stack_map.size_used)

    def fix_invalid_instructions(self, instructions):
        ''' Fix invalid instructions.

        E.g. mov instructions that use a memory address as both
        the source and destination
        '''
        updated_instructions = []
        for instr in instructions:
            updated_instructions += self.fix_invalid_instruction(instr)
        return updated_instructions

    def fix_invalid_instruction(self, instr):
        match instr:
            case assembly.Mov():
                return self.fix_mov(instr)
            case assembly.Movsx():
                return self.fix_movsx(instr)
            case assembly.Cmp():
                return self.fix_cmp(instr)
            case assembly.Idiv():
                return self.fix_idiv(instr)
            case assembly.Binary():
                return self.fix_binary(instr)
            case assembly.Push():
                return self.fix_push(instr)
            case _:
                return [instr]

    def fix_mov(self, instr: assembly.Mov) -> list:
        assembly_type = instr.assembly_type
        src = instr.src
        dst = instr.dst
        if is_mem(src) and is_mem(dst):
            return [
                assembly.Mov(assembly_type, src, r10),
                assembly.Mov(assembly_type, r10, dst),
            ]
        elif is_large_imm(src) and is_mem(dst):
            return [
                assembly.Mov(assembly_type, src, r10),
                assembly.Mov(assembly_type, r10, dst),
            ]
        else:
            return [instr]

    def fix_movsx(self, instr: assembly.Movsx) -> list:
        src = instr.src
        dst = instr.dst
        # It can't use a memory address as the destination or and immediate as
        # the source
        if is_immediate(src) and is_mem(dst):
            return [
                assembly.Mov(longword, src, r10),
                assembly.Movsx(r10, r11),
                assembly.Mov(quadword, r11, dst),
            ]
        elif is_immediate(src):
            return [
                assembly.Mov(longword, src, r10),
                assembly.Movsx(r10, dst),
            ]
        elif is_mem(dst):
            return [
                assembly.Movsx(src, r11),
                assembly.Mov(quadword, r11, dst),
            ]
        else:
            return [instr]

    def fix_cmp(self, instr: assembly.Cmp) -> list:
        assembly_type = instr.assembly_type
        left = instr.left
        right = instr.right
        instructions = []

        if is_mem(left) and is_mem(right):
            instructions.append(assembly.Mov(assembly_type, left, r10))
            left = r10
        elif is_large_imm(left):
            instructions.append(assembly.Mov(assembly_type, left, r10))
            left = r10

        if is_immediate(right):
            instructions.append(assembly.Mov(assembly_type, right, r11))
            right = r11

        instructions.append(assembly.Cmp(assembly_type, left, right))
        return instructions

    def fix_idiv(self, instr: assembly.Idiv) -> list:
        assembly_type = instr.assembly_type
        operand = instr.operand
        if is_immediate(operand):
            return [
                assembly.Mov(assembly_type, operand, r10),
                assembly.Idiv(assembly_type, r10),
            ]
        else:
            return [instr]

    def fix_binary(self, instr: assembly.Binary) -> list:
        op = instr.binary_operator
        assembly_type = instr.assembly_type
        src = instr.src
        dst = instr.dst

        prefix = []
        suffix = []

        if op == assembly.Mult() and is_mem(dst):
            # It's important that Mult is handled differently from other
            # binary operations because it can't have a memory address
            # in the destination
            prefix.append(assembly.Mov(assembly_type, dst, r11))
            suffix.append(assembly.Mov(assembly_type, r11, dst))
            dst = r11

        if is_mem(src) and is_mem(dst):
            prefix.append(assembly.Mov(assembly_type, src, r10))
            src = r10

        if is_large_imm(src) and is_arith(op):
            # The Add, Sub, and Mult instructions can't handle immediate values
            # that don't fit in an int
            prefix.append(assembly.Mov(assembly_type, src, r10))
            src = r10

        instr = assembly.Binary(op, assembly_type, src, dst)
        return prefix + [instr] + suffix

    def fix_push(self, instr: assembly.Push) -> list:
        operand = instr.operand
        if is_large_imm(operand):
            return [
                assembly.Mov(quadword, operand, r10),
                assembly.Push(r10),
            ]
        else:
            return [instr]

    def gen_instructions(self, body: list) -> list:
        result = []
        for instruction in body:
            result += self.gen_instruction(instruction)
        return result

    def gen_instruction(self, instr: tacky.Instruction) -> list:
        ''' returns a list of assembly instructions '''
        assert(isinstance(instr, tacky.Instruction))
        match instr:
            case tacky.Return():
                return self.gen_return(instr)
            case tacky.Unary():
                return self.gen_unary(instr)
            case tacky.Binary():
                return self.gen_binary(instr)
            case tacky.Copy(src, dst):
                a_type = self.a_type_of(src)
                return [
                    assembly.Mov(a_type, self.convert_operand(src), self.convert_operand(dst)),
                ]
            case tacky.Jump(target):
                return [assembly.Jmp(target)]
            case tacky.JumpIfZero(cond, target):
                a_type = self.a_type_of(cond)
                return [
                    assembly.Cmp(a_type, assembly.Immediate(0), self.convert_operand(cond)),
                    assembly.JmpCC('E', target),
                ]
            case tacky.JumpIfNotZero(cond, target):
                a_type = self.a_type_of(cond)
                return [
                    assembly.Cmp(a_type, assembly.Immediate(0), self.convert_operand(cond)),
                    assembly.JmpCC('NE', target),
                ]
            case tacky.Label(name):
                return [assembly.Label(name)]
            case tacky.Call():
                return self.gen_call(instr)
            case tacky.SignExtend():
                return self.gen_sign_extend(instr)
            case tacky.Truncate():
                return self.gen_truncate(instr)
            case _:
                raise Exception(f'unhandled instruction type, {instr}')

    def gen_sign_extend(self, instr: tacky.SignExtend) -> list:
        src = self.convert_operand(instr.src)
        dst = self.convert_operand(instr.dst)
        return [assembly.Movsx(src, dst)]

    def gen_truncate(self, instr: tacky.Truncate) -> list:
        src = self.convert_operand(instr.src)
        if isinstance(src, assembly.Immediate):
            truncated = src.value & (2**32 - 1)
            src = assembly.Immediate(truncated)
        dst = self.convert_operand(instr.dst)
        return [assembly.Mov(longword, src, dst)]

    def gen_call(self, instr: tacky.Call) -> list:
        register_args, stack_args = (instr.arg_vals[:6], instr.arg_vals[6:])
        stack_padding = 8 * (len(stack_args) % 2)

        instructions = []

        # Add stack padding so the alignment comes out right
        if stack_padding > 0:
            allocate = assembly.Binary(
                assembly.Sub(),
                quadword,
                assembly.Immediate(stack_padding),
                assembly.Register('SP')
            )
            instructions.append(allocate)

        # Pass the first 6 arguments in registers
        for (arg, register) in zip(register_args, self.arg_registers):
            assembly_arg = self.convert_operand(arg)
            a_type = self.a_type_of(arg)
            instructions.append(assembly.Mov(a_type, assembly_arg, assembly.Register(register)))

        # Pass the remaining arguments on the stack
        for arg in stack_args[::-1]:
            assembly_arg = self.convert_operand(arg)
            a_type = self.a_type_of(arg)
            is_register = isinstance(assembly_arg, assembly.Register)
            is_immediate = isinstance(assembly_arg, assembly.Immediate)
            if is_register or is_immediate or a_type == quadword:
                instructions.append(assembly.Push(assembly_arg))
            else:
                # Only longword from memory need to be extended to 64 bits before being pushed
                instructions.append(assembly.Mov(a_type, assembly_arg, assembly.Register('AX')))
                instructions.append(assembly.Push(assembly.Register('AX')))

        # The actual function call
        instructions.append(assembly.Call(instr.func_name))

        # Clean up the stack
        bytes_to_remove = 8 * len(stack_args) + stack_padding
        if bytes_to_remove > 0:
            deallocate = assembly.Binary(
                assembly.Add(),
                quadword,
                assembly.Immediate(bytes_to_remove),
                assembly.Register('SP')
            )
            instructions.append(deallocate)

        # Move the result to the correct destination
        assembly_dst = self.convert_operand(instr.dst)
        a_type = self.a_type_of(instr.dst)
        instructions.append(assembly.Mov(a_type, assembly.Register('AX'), assembly_dst))

        return instructions

    def gen_return(self, instr: tacky.Return) -> list:
        src = self.convert_operand(instr.val)
        a_type = self.a_type_of(instr.val)
        return [
            assembly.Mov(a_type, src, assembly.Register('AX')),
            assembly.Ret(),
        ]

    def gen_unary(self, instr: tacky.Unary) -> list:
        src = self.convert_operand(instr.src)
        dst = self.convert_operand(instr.dst)

        a_type = self.a_type_of(instr.src)

        if isinstance(instr.unary_operator, tacky.UnaryNot):
            return [
                assembly.Cmp(a_type, assembly.Immediate(0), src),
                # Zero the destination because the 'set' instruction only
                # updates the lowest 8 bits.
                assembly.Mov(a_type, assembly.Immediate(0), dst),
                assembly.SetCC('E', dst),
            ]

        op = self.convert_unary_operator(instr.unary_operator)
        return [
            assembly.Mov(a_type, src, dst),
            assembly.Unary(op, a_type, dst),
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

        a_type = self.a_type_of(instr.left)

        match instr.operator:
            case tacky.BinaryAdd() | tacky.BinarySubtract() | tacky.BinaryMultiply():
                op = self.convert_binary_operator(instr.operator)
                return [
                    assembly.Mov(a_type, left, dst),
                    assembly.Binary(op, a_type, right, dst),
                ]
            case tacky.BitOr() | tacky.BitXor() | tacky.BitAnd():
                op = self.convert_binary_operator(instr.operator)
                return [
                    assembly.Mov(a_type, left, dst),
                    assembly.Binary(op, a_type, right, dst),
                ]
            case tacky.ShiftLeft() | tacky.ShiftRight():
                op = self.convert_binary_operator(instr.operator)
                return [
                    assembly.Mov(a_type, left, dst),
                    assembly.Mov(a_type, right, assembly.Register('CX')),
                    assembly.Binary(op, a_type, assembly.Register('CX'), dst),
                ]
            case tacky.BinaryDivide():
                return [
                    assembly.Mov(a_type, left, assembly.Register('AX')),
                    assembly.Cdq(a_type),
                    assembly.Idiv(a_type, right),
                    assembly.Mov(a_type, assembly.Register('AX'), dst),
                ]
            case tacky.BinaryRemainder():
                return [
                    assembly.Mov(a_type, left, assembly.Register('AX')),
                    assembly.Cdq(a_type),
                    assembly.Idiv(a_type, right),
                    assembly.Mov(a_type, assembly.Register('DX'), dst),
                ]
            case tacky.Less() | tacky.LessEqual() | tacky.Equals() | \
                 tacky.NotEquals() | tacky.Greater() | tacky.GreaterEqual():
                comparison = self.convert_comparison(instr.operator)
                return [
                    assembly.Cmp(a_type, right, left),
                    assembly.Mov(a_type, assembly.Immediate(0), dst),
                    assembly.SetCC(comparison, dst),
                ]
            case _:
                raise Exception(f'unhandled binary expression op {instr.operator}')

    def a_type_of(self, value: tacky.Value):
        return self.sym_type_to_a_type(self.value_type(value))

    def value_type(self, value: tacky.Value) -> syntax.Type:
        match value:
            case tacky.Constant(tacky.ConstInt(value)):
                return syntax.Int()
            case tacky.Constant(tacky.ConstLong(value)):
                return syntax.Long()
            case tacky.Constant(_):
                assert(False)
            case tacky.Identifier(name):
                return self.symbols[name].type
            case _:
                raise Exception(f'unexpected value {value}')

    def sym_type_to_a_type(self, sym_type: syntax.Type):
        match sym_type:
            case syntax.Int():
                return longword
            case syntax.Long():
                return quadword
            case _:
                raise Exception(f'unexpected type {sym_type} for {name}')

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
            case tacky.Constant(const):
                return assembly.Immediate(const.value)
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


def is_immediate(operand: assembly.Operand):
    match operand:
        case assembly.Immediate():
            return True
        case _:
            return False


def is_large_imm(operand: assembly.Operand):
    match operand:
        case assembly.Immediate(n):
            return n > (2**31 - 1)
        case _:
            return False


def is_arith(op):
    match op:
        case assembly.Add():
            return True
        case assembly.Sub():
            return True
        case assembly.Mult():
            return True
        case _:
            return False


class StackMap:
    '''This is used to figure out where each pseudo register gets stored in a
    function's stack frame'''
    def __init__(self, asm_symbols):
        self.asm_symbols = asm_symbols
        self.size_used = 0
        self.pseudo_registers = {}

    def convert_pseudo_register(self, operand):
        if not isinstance(operand, assembly.Pseudo):
            return operand

        name = operand.name
        if name in self.pseudo_registers:
            return assembly.Stack(self.pseudo_registers[name])

        entry = self.asm_symbols[name]
        assert(isinstance(entry, assembly.ObjEntry))

        if entry.is_static:
            return assembly.Data(name)

        location = self._get_next_location(entry)
        self.pseudo_registers[name] = location
        return assembly.Stack(location)

    def _get_next_location(self, entry):
        size = entry.assembly_type.bytes()
        self.size_used += size
        if self.size_used % size != 0:
            self.size_used += size - (self.size_used % size)
        return -1 * self.size_used
