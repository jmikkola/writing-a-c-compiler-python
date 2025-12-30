import sys

import lexer
import parser
import codegen
import emit


def main(args):
    stage = 'all'
    if '--codegen' in args:
        stage = 'codegen'
    if '--parse' in args:
        stage = 'parse'
    if '--lex' in args:
        stage = 'lex'

    names = [a for a in args if not a.startswith('-')]
    if not names:
        print('Usage: compiler <filename.c> [--lex|--parse|--codegen]')
        sys.exit(1)
    name = names[0]
    with open(name) as inf:
        text = inf.read()

    tokens = lexer.tokenize(text)
    if stage == 'lex':
        return

    syntax = parser.parse(tokens)
    if stage == 'parse':
        return

    asm = codegen.gen(syntax)
    if stage == 'codegen':
        return

    emit.emit(asm, name)


if __name__ == "__main__":
    main(sys.argv[1:])
