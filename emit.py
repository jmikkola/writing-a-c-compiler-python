from collections import namedtuple

import assembly


INDENT = '    '
byte = assembly.AssemblyType.Byte
longword = assembly.AssemblyType.Longword
quadword = assembly.AssemblyType.Quadword


class Register(namedtuple('Register', ['byte', 'dword', 'qword'])):
    pass


REGISTERS = {
    'AX': Register('%al', '%eax', '%rax'),
    'CX': Register('%cl', '%ecx', '%rcx'),
    'DX': Register('%dl', '%edx', '%rdx'),
    'DI': Register('%dil', '%edi', '%rdi'),
    'SI': Register('%sil', '%esi', '%rsi'),
    'SP': Register('%spl', '%esp', '%rsp'),
    'R8': Register('%r8b', '%r8d', '%r8'),
    'R9': Register('%r9b', '%r9d', '%r9'),
    'R10': Register('%r10b', '%r10d', '%r10'),
    'R11': Register('%r11b', '%r11d', '%r11'),
}


def emit(program: assembly.Program, output_name: str):
    with open(output_name, 'w') as out:
        e = Emit(out)
        e.emit_program(program)


class Emit:
    def __init__(self, out):
        self.out = out

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
            case _:
                assert(False)

    def emit_static_variable(self, var: assembly.StaticVariable):
        alignment = var.alignment
        if var.is_global:
            self.indented('.globl ' + var.name)
        if var.init == 0:
            self.indented('.bss')
            self.indented(f'.align {alignment}')
            self.line(var.name + ':')
            self.indented(f'.zero {alignment}')
        else:
            self.indented('.data')
            self.indented(f'.align {alignment}')
            self.line(var.name + ':')
            if alignment == 4:
                self.indented(f'.long {var.init}')
            else:
                self.indented(f'.quad {var.init}')

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
                src = self.render_operand(src, quadword)
                dst = self.render_operand(dst, longword)
                self.indented(f'movslq {src}, {dst}')

            case assembly.Push(operand):
                operand = self.render_operand(operand, quadword)
                self.indented(f'pushq {operand}')

            case assembly.Unary(unary_operator, assembly_type, operand):
                operation = self.convert_unary_operator(unary_operator)
                operand = self.render_operand(operand, assembly_type)
                suffix = self.suffix_for(assembly_type)
                self.indented(f'{operation}{suffix} {operand}')

            case assembly.Binary(binary_operator, assembly_type, src, dst):
                operation = self.convert_binary_operator(binary_operator)
                src = self.render_operand(src, assembly_type)
                dst = self.render_operand(dst, assembly_type)
                suffix = self.suffix_for(assembly_type)
                self.indented(f'{operation}{suffix} {src}, {dst}')

            case assembly.Cdq(assembly_type):
                if assembly_type == longword:
                    self.indented('cdq')
                else:
                    self.indented('cqo')

            case assembly.Idiv(assembly_type, operand):
                operand = self.render_operand(operand, assembly_type)
                suffix = self.suffix_for(assembly_type)
                self.indented(f'idiv{suffix} {operand}')

            case assembly.Cmp(assembly_type, left, right):
                left = self.render_operand(left, assembly_type)
                right = self.render_operand(right, assembly_type)
                suffix = self.suffix_for(assembly_type)
                self.indented(f'cmp{suffix} {left}, {right}')

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

            case _:
                raise Exception(f'unhandled instruction type {instruction}')

    def suffix_for(self, assembly_type) -> str:
        if assembly_type == quadword:
            return 'q'
        else:
            return 'l'

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
            case _:
                raise Exception(f'invalid binary operation to convert to an instruction {binary_operator}')

    def render_operand(self, operand: assembly.Operand, assembly_type: assembly.AssemblyType):
        match operand:
            case assembly.Immediate(value):
                return '$' + str(value)
            case assembly.Register(reg):
                if assembly_type == byte:
                    return REGISTERS[reg].byte
                elif assembly_type == quadword:
                    return REGISTERS[reg].qword
                else:
                    assert(assembly_type == longword)
                    return REGISTERS[reg].dword
            case assembly.Pseudo():
                raise Exception('bug - there should not be a pseudo register by this phase')
            case assembly.Stack(offset):
                return f'{offset}(%rbp)'
            case assembly.Data(name):
                return f'{name}(%rip)'
            case _:
                raise Exception(f'unhandled operand type {operand}')

    def line(self, text):
        self.out.write(text)
        self.out.write('\n')

    def indented(self, text):
        self.out.write(INDENT)
        self.line(text)
