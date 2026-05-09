import argparse
from dataclasses import dataclass


@dataclass
class Args:
    name: str
    stage: str
    print_output: bool
    object: bool
    libraries: list
    emit_assembly: bool

    @classmethod
    def parse(cls, args):
        parser = argparse.ArgumentParser(description='C compiler')
        parser.add_argument('name', metavar='filename.c', help='source file to compile')

        parser.add_argument('-c', dest='object', action='store_true', help='compile to object file')
        parser.add_argument('-l', dest='libraries', action='append', default=[], metavar='library')
        parser.add_argument('-S', dest='emit_assembly', action='store_true', help='emit assembly (a .s file)')

        stage_group = parser.add_mutually_exclusive_group()
        stage_group.add_argument('--lex', dest='stage', action='store_const', const='lex')
        stage_group.add_argument('--parse', dest='stage', action='store_const', const='parse')
        stage_group.add_argument('--validate', dest='stage', action='store_const', const='validate')
        stage_group.add_argument('--tacky', dest='stage', action='store_const', const='tacky')
        stage_group.add_argument('--codegen', dest='stage', action='store_const', const='codegen')
        stage_group.add_argument('--emit', dest='stage', action='store_const', const='emit')

        parser.add_argument('--print', dest='print_output', action='store_true', help='print output')

        parsed = parser.parse_args(args)
        stage = parsed.stage if parsed.stage else 'all'

        return Args(
            name=parsed.name,
            stage=stage,
            print_output=parsed.print_output,
            object=parsed.object,
            libraries=parsed.libraries,
            emit_assembly=parsed.emit_assembly,
        )
