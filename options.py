import argparse
from collections import namedtuple


class Args(namedtuple('Args', ['name', 'stage', 'print_output', 'object', 'libraries'])):
    @classmethod
    def parse(cls, args):
        parser = argparse.ArgumentParser(description='C compiler')
        parser.add_argument('name', metavar='filename.c', help='source file to compile')

        parser.add_argument('-c', dest='object', action='store_true', help='compile to object file')
        parser.add_argument('-l', dest='libraries', action='append', default=[], metavar='library')

        stage_group = parser.add_mutually_exclusive_group()
        stage_group.add_argument('--parse', dest='stage', action='store_const', const='parse')
        stage_group.add_argument('--validate', dest='stage', action='store_const', const='validate')
        stage_group.add_argument('--tacky', dest='stage', action='store_const', const='tacky')
        stage_group.add_argument('--codegen', dest='stage', action='store_const', const='codegen')
        stage_group.add_argument('--emit', dest='stage', action='store_const', const='emit')

        parser.add_argument('--print', dest='print_output', action='store_true', help='print output')

        parsed = parser.parse_args(args)
        stage = parsed.stage if parsed.stage else 'all'

        return Args(parsed.name, stage, parsed.print_output, parsed.object, parsed.libraries)
