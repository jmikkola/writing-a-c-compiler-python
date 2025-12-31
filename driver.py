import pathlib
import subprocess

import lexer
import parser
import to_ir
import codegen
import emit


def run_compiler(name, stage, print_output=False):
    preprocessed_file = name.replace('.c', '.i')
    assembly_file = name.replace('.c', '.s')
    compiled_file = name.replace('.c', '')

    to_clean_up = [preprocessed_file]
    if stage == 'all':
        to_clean_up.append(assembly_file)

    preprocess(name, preprocessed_file)
    compile(stage, preprocessed_file, assembly_file, print_output)
    if stage == 'all':
        assemble_and_link(assembly_file, compiled_file)

    # Clean up temporary files
    # (if the previous stages errored out, leave the files alone for debugging)
    for name in to_clean_up:
        path = pathlib.Path(name)
        path.unlink()


def preprocess(c_file, preprocessed_file):
    subprocess.run(['gcc', '-E', '-P', c_file, '-o', preprocessed_file], check=True)


def compile(stage, preprocessed_file, assembly_file, print_output):
    with open(preprocessed_file) as inf:
        text = inf.read()

    tokens = lexer.tokenize(text)
    if stage == 'lex':
        if print_output:
            for t in tokens:
                print(t)
        return

    syntax = parser.parse(tokens)
    if stage == 'parse':
        if print_output:
            print(syntax)
        return

    ir = to_ir.to_ir(syntax)
    if stage == 'tacky':
        if print_output:
            print(ir.pretty_print())
        return

    asm = codegen.gen(ir)
    if stage == 'codegen':
        if print_output:
            print(asm.pretty_print())
        return

    emit.emit(asm, assembly_file)
    if print_output:
        with open(assembly_file, 'r') as inf:
            print(inf.read())


def assemble_and_link(assembly_file, compiled_file):
    subprocess.run(['gcc', assembly_file, '-o', compiled_file], check=True)
