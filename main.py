import sys

import driver


def main(args):
    name, stage, print_output = parse_args(args)
    driver.run_compiler(name, stage, print_output)


def parse_args(args):
    stage = 'all'
    if '--codegen' in args:
        stage = 'codegen'
    if '--tacky' in args:
        stage = 'tacky'
    if '--parse' in args:
        stage = 'parse'
    if '--lex' in args:
        stage = 'lex'

    print_output = '--print' in args

    names = [a for a in args if not a.startswith('-')]
    if not names:
        print('Usage: compiler <filename.c> [--lex|--parse|--codegen]')
        sys.exit(1)
    name = names[0]

    return (name, stage, print_output)


if __name__ == "__main__":
    main(sys.argv[1:])
