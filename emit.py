from collections import namedtuple

import assembly
import symbol


INDENT = '    '
byte = assembly.Byte()
longword = assembly.Longword()
quadword = assembly.Quadword()
double = assembly.Double()


class Register(namedtuple('Register', ['byte', 'dword', 'qword'])):
    pass


REGISTERS = {
    'AX': Register('%al', '%eax', '%rax'),
    'CX': Register('%cl', '%ecx', '%rcx'),
    'DX': Register('%dl', '%edx', '%rdx'),
    'DI': Register('%dil', '%edi', '%rdi'),
    'SI': Register('%sil', '%esi', '%rsi'),
    'SP': Register('%spl', '%esp', '%rsp'),
    'BP': Register('%bpl', '%ebp', '%rbp'),
    'R8': Register('%r8b', '%r8d', '%r8'),
    'R9': Register('%r9b', '%r9d', '%r9'),
    'R10': Register('%r10b', '%r10d', '%r10'),
    'R11': Register('%r11b', '%r11d', '%r11'),
}


def emit(program: assembly.Program, asm_symbols: dict, output_name: str):
    with open(output_name, 'w') as out:
        e = Emit(out, asm_symbols)
        e.emit_program(program)


class Emit:
    def __init__(self, out, asm_symbols):
        self.out = out
        self.asm_symbols = asm_symbols

    def emit_program(self, program):
        for decl in program.top_level:
            self.emit_top_level(decl)
        self.indented('.section  .note.GNU-stack,"",@progbits')

    def emit_top_level(self, decl):
        match decl:
            case assembly.Function():
                self.emit_function(decl)
            case assembly.StaticVariable():
                self.emit_static_variable(decl)
            case assembly.StaticConstant():
                self.emit_static_constant(decl)
            case _:
                raise Exception(f'unhandled declaration {decl}')

    def emit_static_constant(self, var: assembly.StaticConstant):
        alignment = var.alignment
        self.indented('.section .rodata')
        self.indented(f'.align {alignment}')
        self.line('L' + var.name + ':')
        init = var.init
        if isinstance(init, symbol.ZeroInit):
            self.indented(f'.zero {init.bytes}')
            return

        value = init.value
        if alignment == 4:
            self.indented(f'.long {value}')
        elif isinstance(init, symbol.DoubleInit):
            self.indented(f'.double {value:.18e}')
        else:
            self.indented(f'.quad {value}')

    def emit_static_variable(self, var: assembly.StaticVariable):
        alignment = var.alignment
        if var.is_global:
            self.indented('.globl ' + var.name)
        inits = var.inits
        if inits == [0]:
            self.indented('.bss')
            self.indented(f'.align {alignment}')
            self.line(var.name + ':')
            self.indented(f'.zero {alignment}')
        else:
            self.indented('.data')
            self.indented(f'.align {alignment}')
            self.line(var.name + ':')
            for init in inits:
                if isinstance(init, symbol.ZeroInit):
                    self.indented(f'.zero {init.bytes}')
                elif alignment == 4:
                    self.indented(f'.long {init.value}')
                elif isinstance(init, symbol.DoubleInit):
                    self.indented(f'.double {init.value:.18e}')
                else:
                    self.indented(f'.quad {init.value}')

    def emit_function(self, function: assembly.Function):
        if function.is_global:
            self.indented('.globl ' + function.name)
        self.indented('.text')
        self.line(function.name + ':')
        self.indented('pushq %rbp')
        self.indented('movq %rsp, %rbp')
        for instruction in function.instructions:
            self.emit_instruction(instruction)

    def emit_instruction(self, instruction: assembly.Instruction):
        assert(isinstance(instruction, assembly.Instruction))
        match instruction:
            case assembly.Ret():
                self.indented('movq %rbp, %rsp')
                self.indented('popq %rbp')
                self.indented('ret')

            case assembly.Call(func):
                self.indented(f'call {func}@PLT')

            case assembly.Mov(assembly_type, src, dst):
                src = self.render_operand(src, assembly_type)
                dst = self.render_operand(dst, assembly_type)
                suffix = self.suffix_for(assembly_type)
                self.indented(f'mov{suffix} {src}, {dst}')

            case assembly.Movsx(src, dst):
                src = self.render_operand(src, longword)
                dst = self.render_operand(dst, quadword)
                self.indented(f'movslq {src}, {dst}')

            case assembly.Lea(src, dst):
                src = self.render_operand(src, quadword)
                dst = self.render_operand(dst, quadword)
                self.indented(f'leaq {src}, {dst}')

            case assembly.Push(operand):
                operand = self.render_operand(operand, quadword)
                self.indented(f'pushq {operand}')

            case assembly.Unary(unary_operator, assembly_type, operand):
                operation = self.convert_unary_operator(unary_operator)
                operand = self.render_operand(operand, assembly_type)
                suffix = self.suffix_for(assembly_type)
                self.indented(f'{operation}{suffix} {operand}')

            case assembly.Binary(binary_operator, assembly_type, src, dst):
                if assembly_type == double and binary_operator == assembly.BitXor():
                    asm = 'xorpd'
                elif assembly_type == double and binary_operator == assembly.Mult():
                    asm = 'mulsd'
                else:
                    operation = self.convert_binary_operator(binary_operator)
                    suffix = self.suffix_for(assembly_type)
                    asm = operation + suffix
                src = self.render_operand(src, assembly_type)
                dst = self.render_operand(dst, assembly_type)
                self.indented(f'{asm} {src}, {dst}')

            case assembly.Cdq(assembly_type):
                if assembly_type == longword:
                    self.indented('cdq')
                else:
                    self.indented('cqo')

            case assembly.Idiv(assembly_type, operand):
                operand = self.render_operand(operand, assembly_type)
                suffix = self.suffix_for(assembly_type)
                self.indented(f'idiv{suffix} {operand}')

            case assembly.Div(assembly_type, operand):
                operand = self.render_operand(operand, assembly_type)
                suffix = self.suffix_for(assembly_type)
                self.indented(f'div{suffix} {operand}')

            case assembly.Cmp(assembly_type, left, right):
                left = self.render_operand(left, assembly_type)
                right = self.render_operand(right, assembly_type)
                suffix = self.suffix_for(assembly_type)
                asm = 'cmp'
                if assembly_type == double:
                    asm = 'comi'
                self.indented(f'{asm}{suffix} {left}, {right}')

            case assembly.Jmp(label):
                label = self.add_label_prefix(label)
                self.indented(f'jmp {label}')

            case assembly.JmpCC(cond_code, label):
                cond = cond_code.lower()
                label = self.add_label_prefix(label)
                self.indented(f'j{cond} {label}')

            case assembly.SetCC(cond_code, operand):
                cond = cond_code.lower()
                operand = self.render_operand(operand, byte)
                self.indented(f'set{cond} {operand}')

            case assembly.Label(label):
                label = self.add_label_prefix(label)
                self.line(label + ':')

            case assembly.Cvttsd2si(assembly_type, src, dst):
                src = self.render_operand(src, double)
                dst = self.render_operand(dst, assembly_type)
                suffix = self.suffix_for(assembly_type)
                self.indented(f'cvttsd2si{suffix} {src}, {dst}')

            case assembly.Cvtsi2sd(assembly_type, src, dst):
                src = self.render_operand(src, assembly_type)
                dst = self.render_operand(dst, double)
                suffix = self.suffix_for(assembly_type)
                self.indented(f'cvtsi2sd{suffix} {src}, {dst}')

            case _:
                raise Exception(f'unhandled instruction type {instruction}')

    def suffix_for(self, assembly_type) -> str:
        if assembly_type == quadword:
            return 'q'
        if assembly_type == double:
            return 'sd'
        if assembly_type == longword:
            return 'l'
        if isinstance(assembly_type, assembly.ByteArray):
            return 'q'
        raise Exception(f'unexpected assembly type {assembly_type}')

    def add_label_prefix(self, name):
        return '.L' + name

    def convert_unary_operator(self, unary_operator: assembly.UnaryOperator) -> str:
        match unary_operator:
            case assembly.Not():
                return 'not'
            case assembly.Neg():
                return 'neg'
            case _:
                raise Exception(f'unhandled unary operator {unary_operator}')

    def convert_binary_operator(self, binary_operator: assembly.BinaryOperator) -> str:
        match binary_operator:
            case assembly.Add():
                return 'add'
            case assembly.Sub():
                return 'sub'
            case assembly.Mult():
                return 'imul'
            case assembly.DivDouble():
                return 'div'
            case assembly.BitAnd():
                return 'and'
            case assembly.BitOr():
                return 'or'
            case assembly.BitXor():
                return 'xor'
            case assembly.ShiftLeft():
                return 'sal'
            case assembly.ShiftRight():
                return 'sar'
            case assembly.ShiftRightLogical():
                return 'shr'
            case _:
                raise Exception(f'invalid binary operation to convert to an instruction {binary_operator}')

    def render_operand(self, operand: assembly.Operand, assembly_type: assembly.AssemblyType):
        match operand:
            case assembly.Immediate(value):
                return '$' + str(value)
            case assembly.Register(reg):
                if assembly_type == double:
                    return '%' + reg.lower()
                elif assembly_type == byte:
                    return REGISTERS[reg].byte
                elif assembly_type == quadword:
                    return REGISTERS[reg].qword
                elif assembly_type == longword:
                    return REGISTERS[reg].dword
                elif isinstance(assembly_type, assembly.ByteArray):
                    return REGISTERS[reg].qword
                else:
                    raise Exception(f'unhandled assembly type: {assembly_type}')
            case assembly.Pseudo():
                raise Exception('bug - there should not be a pseudo register by this phase')
            case assembly.Memory(reg, offset):
                register = REGISTERS[reg].qword
                if offset == 0:
                    return f'({register})'
                return f'{offset}({register})'
            case assembly.Data(name):
                if self.asm_symbols[name].is_constant:
                    name = 'L' + name
                return f'{name}(%rip)'
            case assembly.Indexed(base_reg, index_reg, scale):
                base = REGISTERS[base_reg].qword
                index = REGISTERS[index_reg].qword
                return f'({base}, {index}, {scale})'
            case _:
                raise Exception(f'unhandled operand type {operand}')

    def line(self, text):
        self.out.write(text)
        self.out.write('\n')

    def indented(self, text):
        self.out.write(INDENT)
        self.line(text)
