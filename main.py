import sys

import lexer


def main(args):
    if not args:
        print('Usage: compiler <filename.c>')
        sys.exit(1)
    with open(args[0]) as inf:
        text = inf.read()
    tokens = lexer.tokenize(text)
    for t in tokens:
        print(f'{t.kind} {t.text}')


if __name__ == "__main__":
    main(sys.argv[1:])
