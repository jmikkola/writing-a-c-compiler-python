import assembly


INDENT = '    '


def emit(program: assembly.Program, name: str):
    output_name = name.replace('.c', '.s')
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
        for instruction in function.instructions:
            self.emit_instruction(instruction)

    def emit_instruction(self, instruction: assembly.Instruction):
        match instruction:
            case assembly.Ret():
                self.indented('ret')
            case assembly.Mov(src, dst):
                src = self.render_operand(src)
                dst = self.render_operand(dst)
                self.indented(f'movl {src}, {dst}')
            case _:
                raise Exception(f'unhandled instruction type {instruction}')

    def render_operand(self, operand: assembly.Operand):
        match operand:
            case assembly.Immediate(value):
                return '$' + str(value)
            case assembly.Register():
                return '%eax'
            case _:
                raise Exception(f'unhandled operand type {operand}')

    def line(self, text):
        self.out.write(text)
        self.out.write('\n')

    def indented(self, text):
        self.out.write(INDENT)
        self.line(text)
