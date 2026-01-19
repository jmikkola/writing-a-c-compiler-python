import re
import itertools

from token import Token


IDENTIFIER = re.compile(r'[a-zA-Z_]\w*\b')
# Digits optionally ending in l, u, ul, or lu in any mix of case
CONSTANT   = re.compile(r'[0-9]+([lL]|[uU]|[uU][lL]|[lL][uU])?\b')

LINE_COMMENT = re.compile(r'//[^\n]*\n?')
# The *? is non-greedy, so it will stop at the first */ not the last one
MULTILINE_COMENT = re.compile(r'/\*.*?\*/')

KEYWORDS = [
    'int', 'void', 'long', 'signed', 'unsigned',
    'return', 'if', 'else', 'goto',
    'do', 'while', 'break', 'continue', 'for',
    'switch', 'case', 'default',
    'static', 'extern',
]

THREE_PART_SYMBOLS = [
    '>>=', '<<=',
]
TWO_PART_SYMBOLS = [
    '--', '++',
    '<<', '>>',
    '&&', '||',
    '>=', '==', '!=', '<=',
    '+=', '-=', '*=', '/=', '%=',
    '&=', '|=', '^=',
]
PUNCTUATION = [
    '(', ')', '{', '}', ';',
    '+', '-', '*', '/', '%', '~',
    '&', '|', '^', '!', '<', '>',
    '=', '?', ':', ',',
]


def tokenize(text):
    tokens = []
    while text:
        if text[0].isspace():
            text = text[1:]
            continue

        comment_match = LINE_COMMENT.match(text)
        if comment_match is None:
            comment_match = MULTILINE_COMENT.match(text)
        if comment_match is not None:
            comment = comment_match.group()
            text = text[len(comment):]
            continue

        identifier_match = IDENTIFIER.match(text)
        if identifier_match is not None:
            identifier = identifier_match.group()
            text = text[len(identifier):]
            if identifier in KEYWORDS:
                tokens.append(Token('keyword', identifier))
            else:
                tokens.append(Token('identifier', identifier))
            continue

        constant_match = CONSTANT.match(text)
        if constant_match is not None:
            constant = constant_match.group()
            text = text[len(constant):]
            tokens.append(Token('constant', constant))
            continue

        found_punctuation = False
        for p in itertools.chain(THREE_PART_SYMBOLS, TWO_PART_SYMBOLS, PUNCTUATION):
            if text.startswith(p):
                found_punctuation = True
                text = text[len(p):]
                tokens.append(Token(p, p))
                break
        if found_punctuation:
            continue

        raise Exception(f'Invalid token starting at {text[:20]}')

    return tokens
