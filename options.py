from collections import namedtuple
import sys


class Args(namedtuple('Args', ['name', 'stage', 'print_output', 'object'])):
    @classmethod
    def parse(cls, args):
        stage = 'all'
        if '--emit' in args:
            stage = 'emit'
        if '--codegen' in args:
            stage = 'codegen'
        if '--tacky' in args:
            stage = 'tacky'
        if '--validate' in args:
            stage = 'validate'
        if '--parse' in args:
            stage = 'parse'
        if '--lex' in args:
            stage = 'lex'

        print_output = '--print' in args

        object = '-c' in args

        names = [a for a in args if not a.startswith('-')]
        if not names:
            print('Usage: compiler <filename.c> [--lex|--parse|--codegen]')
            sys.exit(1)
        name = names[0]

        return Args(name, stage, print_output, object)
