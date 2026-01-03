from collections import namedtuple

import assembly


INDENT = '    '


class Register(namedtuple('Register', ['byte', 'dword', 'qword'])):
    pass


REGISTERS = {
    'AX': Register('%al', '%eax', '%rax'),
    'CX': Register('%cl', '%ecx', '%rcx'),
    'DX': Register('%dl', '%edx', '%rdx'),
    'DI': Register('%dil', '%edi', '%rdi'),
    'SI': Register('%sil', '%esi', '%rsi'),
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
        for f in program.functions:
            self.emit_function(f)
        self.indented('.section  .note.GNU-stack,"",@progbits')

    def emit_function(self, function: assembly.Function):
        self.indented('.globl ' + function.name)
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
            case assembly.Mov(src, dst):
                src = self.render_operand(src)
                dst = self.render_operand(dst)
                self.indented(f'movl {src}, {dst}')
            case assembly.AllocateStack(size):
                self.indented(f'subq ${size}, %rsp')
            case assembly.DeallocateStack(size):
                self.indented(f'addq ${size}, %rsp')
            case assembly.Push(operand):
                operand = self.render_operand(operand, qword=True)
                self.indented(f'pushq {operand}')
            case assembly.Unary(unary_operator, operand):
                operation = self.convert_unary_operator(unary_operator)
                operand = self.render_operand(operand)
                self.indented(f'{operation} {operand}')
            case assembly.Binary(binary_operator, src, dst):
                operation = self.convert_binary_operator(binary_operator)
                src = self.render_operand(src)
                dst = self.render_operand(dst)
                self.indented(f'{operation} {src}, {dst}')
            case assembly.Cdq():
                self.indented('cdq')
            case assembly.Idiv(operand):
                operand = self.render_operand(operand)
                self.indented(f'idivl {operand}')
            case assembly.Cmp(left, right):
                left = self.render_operand(left)
                right = self.render_operand(right)
                self.indented(f'cmpl {left}, {right}')
            case assembly.Jmp(label):
                label = self.add_label_prefix(label)
                self.indented(f'jmp {label}')
            case assembly.JmpCC(cond_code, label):
                cond = cond_code.lower()
                label = self.add_label_prefix(label)
                self.indented(f'j{cond} {label}')
            case assembly.SetCC(cond_code, operand):
                cond = cond_code.lower()
                operand = self.render_operand(operand, byte_size=True)
                self.indented(f'set{cond} {operand}')
            case assembly.Label(label):
                label = self.add_label_prefix(label)
                self.line(label + ':')
            case _:
                raise Exception(f'unhandled instruction type {instruction}')

    def add_label_prefix(self, name):
        return '.L' + name

    def convert_unary_operator(self, unary_operator: assembly.UnaryOperator) -> str:
        match unary_operator:
            case assembly.Not():
                return 'notl'
            case assembly.Neg():
                return 'negl'
            case _:
                raise Exception(f'unhandled unary operator {unary_operator}')

    def convert_binary_operator(self, binary_operator: assembly.BinaryOperator) -> str:
        match binary_operator:
            case assembly.Add():
                return 'addl'
            case assembly.Sub():
                return 'subl'
            case assembly.Mult():
                return 'imull'
            case assembly.BitAnd():
                return 'andl'
            case assembly.BitOr():
                return 'orl'
            case assembly.BitXor():
                return 'xorl'
            case assembly.ShiftLeft():
                return 'sall'
            case assembly.ShiftRight():
                return 'sarl'
            case _:
                raise Exception(f'invalid binary operation to convert to an instruction {binary_operator}')

    def render_operand(self, operand: assembly.Operand, byte_size=False, qword=False):
        match operand:
            case assembly.Immediate(value):
                return '$' + str(value)
            case assembly.Register(reg):
                if byte_size:
                    return REGISTERS[reg].byte
                elif qword:
                    return REGISTERS[reg].qword
                else:
                    return REGISTERS[reg].dword
            case assembly.Pseudo():
                raise Exception('bug - there should not be a pseudo register by this phase')
            case assembly.Stack(offset):
                return f'{offset}(%rbp)'
            case _:
                raise Exception(f'unhandled operand type {operand}')

    def line(self, text):
        self.out.write(text)
        self.out.write('\n')

    def indented(self, text):
        self.out.write(INDENT)
        self.line(text)
