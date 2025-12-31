import assembly


INDENT = '    '


def emit(program: assembly.Program, output_name: str):
    with open(output_name, 'w') as out:
        e = Emit(out)
        e.emit_program(program)


class Emit:
    def __init__(self, out):
        self.out = out

    def emit_program(self, program):
        self.emit_function(program.function_definition)
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
            case assembly.Mov(src, dst):
                src = self.render_operand(src)
                dst = self.render_operand(dst)
                self.indented(f'movl {src}, {dst}')
            case assembly.AllocateStack(size):
                self.indented(f'subq ${size}, %rsp')
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
            case _:
                raise Exception(f'unhandled instruction type {instruction}')

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

    def render_operand(self, operand: assembly.Operand):
        match operand:
            case assembly.Immediate(value):
                return '$' + str(value)
            case assembly.Register(reg='AX'):
                return '%eax'
            case assembly.Register(reg='CX'):
                return '%ecx'
            case assembly.Register(reg='DX'):
                return '%edx'
            case assembly.Register(reg='R10'):
                return '%r10d'
            case assembly.Register(reg='R11'):
                return '%r11d'
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
