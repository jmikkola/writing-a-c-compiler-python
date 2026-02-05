from collections import namedtuple


class Token(namedtuple('Token', ['kind', 'text'])):
    pass
