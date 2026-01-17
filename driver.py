import pathlib
import subprocess
import sys

import options
import lexer
import parser
import validator
import to_ir
import codegen
import emit
from errors import WACCException


def run_compiler(args: options.Args):
    name = args.name
    stage = args.stage

    preprocessed_file = name.replace('.c', '.i')
    assembly_file = name.replace('.c', '.s')
    if args.object:
        compiled_file = name.replace('.c', '.o')
    else:
        compiled_file = name.replace('.c', '')

    to_clean_up = [preprocessed_file]
    if stage == 'all':
        to_clean_up.append(assembly_file)

    preprocess(name, preprocessed_file)
    try:
        compile(stage, preprocessed_file, assembly_file, args.print_output)
    except WACCException as e:
        print(e, file=sys.stderr)
        sys.exit(1)

    if stage == 'all':
        assemble_and_link(assembly_file, compiled_file, args.object)

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
            print('\n'.join(syntax.pretty_print()))
        return

    syntax, symbols = validator.validate(syntax)
    if stage == 'validate':
        if print_output:
            print('\n'.join(syntax.pretty_print()))
            for (name, sym) in symbols.items():
                print(f'{name}: {sym}')
        return

    ir = to_ir.to_ir(syntax, symbols)
    if stage == 'tacky':
        if print_output:
            print(ir.pretty_print())
        return

    asm, backend_symbols = codegen.gen(ir, symbols)
    if stage == 'codegen':
        if print_output:
            print(asm.pretty_print())
            for (name, sym) in backend_symbols.items():
                print(f'{name}: {sym}')
        return

    emit.emit(asm, assembly_file)
    if stage == 'emit':
        if print_output:
            with open(assembly_file, 'r') as inf:
                print(inf.read())


def assemble_and_link(assembly_file, compiled_file, object):
    command = ['gcc', assembly_file, '-o', compiled_file]
    if object:
        command.append('-c')
    subprocess.run(command, check=True)
