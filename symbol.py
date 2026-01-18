from collections import namedtuple


class Symbol(namedtuple('Symbol', ['type', 'attrs'])):
    pass


class IdentifierAttributes:
    pass


class FuncAttr(IdentifierAttributes, namedtuple('FuncAttr', ['is_defined', 'is_global'])):
    pass


class StaticAttr(IdentifierAttributes, namedtuple('StaticAttr', ['init', 'is_global'])):
    pass


class LocalAttr(IdentifierAttributes, namedtuple('LocalAttr', [])):
    pass


class InitialValue:
    pass


class Tentative(InitialValue, namedtuple('Tentative', [])):
    pass


class Initial(InitialValue, namedtuple('Initial', ['static_value'])):
    pass


class NoInitializer(InitialValue, namedtuple('NoInitializer', [])):
    pass


class StaticInit:
    pass


class IntInit(StaticInit, namedtuple('IntInit', ['value'])):
    pass


class LongInit(StaticInit, namedtuple('LongInit', ['value'])):
    pass
