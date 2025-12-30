import pathlib
import subprocess

import lexer
import parser
import to_ir
import codegen
import emit


def run_compiler(name, stage):
    preprocessed_file = name.replace('.c', '.i')
    assembly_file = name.replace('.c', '.s')
    compiled_file = name.replace('.c', '')

    to_clean_up = [preprocessed_file]
    if stage == 'all':
        to_clean_up.append(assembly_file)

    preprocess(name, preprocessed_file)
    compile(stage, preprocessed_file, assembly_file)
    if stage == 'all':
        assemble_and_link(assembly_file, compiled_file)

    # Clean up temporary files
    # (if the previous stages errored out, leave the files alone for debugging)
    for name in to_clean_up:
        path = pathlib.Path(name)
        path.unlink()


def preprocess(c_file, preprocessed_file):
    subprocess.run(['gcc', '-E', '-P', c_file, '-o', preprocessed_file], check=True)


def compile(stage, preprocessed_file, assembly_file):
    with open(preprocessed_file) as inf:
        text = inf.read()

    tokens = lexer.tokenize(text)
    if stage == 'lex':
        return

    syntax = parser.parse(tokens)
    if stage == 'parse':
        return

    ir = to_ir.to_ir(syntax)
    if stage == 'tacky':
        return

    asm = codegen.gen(ir)
    if stage == 'codegen':
        return

    emit.emit(asm, assembly_file)


def assemble_and_link(assembly_file, compiled_file):
    subprocess.run(['gcc', assembly_file, '-o', compiled_file], check=True)
