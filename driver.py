import pathlib
import subprocess
import sys
import time
import os
from contextlib import contextmanager

import labels
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
    if stage == 'all' and not args.emit_assembly:
        to_clean_up.append(assembly_file)

    with timed('preprocess'):
        preprocess(name, preprocessed_file)
    try:
        with timed('compile'):
            compile(stage, preprocessed_file, assembly_file, args.print_output)
    except WACCException as e:
        print(e, file=sys.stderr)
        sys.exit(1)

    if stage == 'all' and not args.emit_assembly:
        with timed('assemble_and_link'):
            assemble_and_link(assembly_file, compiled_file, args.object, args.libraries)

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

    with timed('tokenize'):
        tokens = lexer.tokenize(text)
    if stage == 'lex':
        if print_output:
            for t in tokens:
                print(t)
        return

    with timed('parse'):
        syntax = parser.parse(tokens)
    if stage == 'parse':
        if print_output:
            print('\n'.join(syntax.pretty_print()))
        return

    with timed('validate'):
        syntax, symbols, types = validator.validate(syntax)
    if stage == 'validate':
        if print_output:
            print('\n'.join(syntax.pretty_print()))
            print('\n# symbols:')
            for (name, sym) in symbols.items():
                print(f'{name}: {sym}')
        return

    label_gen = labels.Labels()
    with timed('tacky'):
        ir = to_ir.to_ir(syntax, symbols, types, label_gen)
    if stage == 'tacky':
        if print_output:
            print(ir.pretty_print())
            print('\n# symbols:')
            for (name, sym) in symbols.items():
                print(f'{name}: {sym}')
        return

    with timed('codegen'):
        asm, backend_symbols = codegen.gen(ir, symbols, types, label_gen)
    if stage == 'codegen':
        if print_output:
            print(asm.pretty_print())
            print('\n# symbols:')
            for (name, sym) in backend_symbols.items():
                print(f'{name}: {sym}')
        return

    with timed('emit'):
        emit.emit(asm, backend_symbols, assembly_file)
    if stage == 'emit':
        if print_output:
            with open(assembly_file, 'r') as inf:
                print(inf.read())


def assemble_and_link(assembly_file, compiled_file, object, libraries):
    command = ['gcc', assembly_file, '-o', compiled_file]
    if object:
        command.append('-c')
    for lib in libraries:
        command.append('-l' + lib)
    subprocess.run(command, check=True)


enable_timing = os.environ.get('TIMING') is not None


@contextmanager
def timed(name):
    start = time.time()
    yield
    delta = time.time() - start
    if enable_timing:
        print(f'{name} took {delta:.4f}')
