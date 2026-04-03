from collections import namedtuple


class Symbol(namedtuple('Symbol', ['type', 'attrs'])):
    def __str__(self):
        return f'Symbol({self.type}, {self.attrs})'


class IdentifierAttributes:
    pass


class FuncAttr(IdentifierAttributes, namedtuple('FuncAttr', ['is_defined', 'is_global'])):
    pass


class StaticAttr(IdentifierAttributes, namedtuple('StaticAttr', ['init', 'is_global'])):
    pass


class ConstantAttr(IdentifierAttributes, namedtuple('ConstantAttr', ['init'])):
    pass


class LocalAttr(IdentifierAttributes, namedtuple('LocalAttr', [])):
    pass


class InitialValue:
    pass


class Tentative(InitialValue, namedtuple('Tentative', [])):
    pass


class Initial(InitialValue, namedtuple('Initial', ['static_values'])):
    pass


class NoInitializer(InitialValue, namedtuple('NoInitializer', [])):
    pass


class StaticInit:
    pass


class CharInit(StaticInit, namedtuple('CharInit', ['value'])):
    ''' for Char and SChar typed values '''
    pass


class IntInit(StaticInit, namedtuple('IntInit', ['value'])):
    pass


class LongInit(StaticInit, namedtuple('LongInit', ['value'])):
    pass


class UCharInit(StaticInit, namedtuple('UCharInit', ['value'])):
    pass


class UIntInit(StaticInit, namedtuple('UIntInit', ['value'])):
    pass


class ULongInit(StaticInit, namedtuple('ULongInit', ['value'])):
    pass


class DoubleInit(StaticInit, namedtuple('DoubleInit', ['value'])):
    pass


class StringInit(StaticInit, namedtuple('StringInit', ['string', 'null_terminated'])):
    pass


class PointerInit(StaticInit, namedtuple('PointerInit', ['name'])):
    pass


class ZeroInit(StaticInit, namedtuple('ZeroInit', ['bytes'])):
    pass
