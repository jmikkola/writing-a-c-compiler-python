from __future__ import annotations
from collections import namedtuple
from dataclasses import dataclass
from typing import Union, List

##
## Pretty printing
##

def indent(lines):
    return ['  ' + line for line in lines]


def headed(header, lines):
    '''
    Handles both
        if (foo)
          bar();
    and
        if (foo) {
          bar();
        }
    '''
    first = lines[0].strip()
    if first == '{':
        return [f'{header} {{'] + lines[1:]
    else:
        return [header] + indent(lines)


def trailer(lines, trailer):
    last = lines[-1].strip()
    if last == '}':
        return lines[:-1] + [f'}} {trailer}']
    else:
        return lines + [trailer]

##
## Types
##

class Type:
    def pretty_print(self):
        return str(self)

    def __repr__(self):
        return str(self)

    def __eq__(self, other):
        # Super lazy
        return str(self) == str(other)

    def __ne__(self, other):
        return not (self == other)


class Int(Type, namedtuple('Int', [])):
    def __str__(self):
        return 'Int'


class Long(Type, namedtuple('Type', [])):
    def __str__(self):
        return 'Long'


class UInt(Type):
    def __str__(self):
        return 'UInt'


class ULong(Type):
    def __str__(self):
        return 'ULong'


class Double(Type):
    def __str__(self):
        return 'Double'


class Char(Type):
    def __str__(self):
        return 'Char'


class SChar(Type):
    ''' signed char '''
    def __str__(self):
        return 'SChar'


class UChar(Type):
    ''' unsigned char '''
    def __str__(self):
        return 'UChar'


class Void(Type):
    def __str__(self):
        return 'Void'


class Func(Type, namedtuple('Func', ['params', 'ret'])):
    def __str__(self):
        params = ', '.join(str(param) for param in self.params)
        return f'Func(params=[{params}], ret={self.ret})'


class Pointer(Type, namedtuple('Pointer', ['referenced'])):
    def __str__(self):
        referenced = str(self.referenced)
        return f'Pointer({referenced})'


class Array(Type, namedtuple('Array', ['element', 'size'])):
    def __str__(self):
        return f'Array({self.element}, {self.size})'


class Struct(Type, namedtuple('Struct', ['tag'])):
    def __str__(self):
        return f'Struct({self.tag})'


##
## Type declarator
##


# Concrete declarators

@dataclass
class Ident:
    identifier: str


@dataclass
class PointerDeclarator:
    # This needs `from __future__ import annotations` to not have a name error
    declarator: Declarator


@dataclass
class FunDeclarator:
    params: List[ParamInfo]
    decl: Declarator


@dataclass
class ArrayDeclarator:
    element: Declarator
    size: int


Declarator = Union[Ident, PointerDeclarator, FunDeclarator, ArrayDeclarator]


@dataclass
class ParamInfo:
    t: Type
    declarator: Declarator


# Abstract declarators (for cast expressions)

@dataclass
class AbstractPointer:
    declarator: AbstractDeclarator


@dataclass
class AbstractArray:
    declarator: AbstractDeclarator
    size: int


@dataclass
class AbstractBase:
    pass


AbstractDeclarator = Union[AbstractPointer, AbstractBase, AbstractArray]


##
## Program structure
##

class Program(namedtuple('Program', ['declarations'])):
    def pretty_print(self):
        lines = []
        for d in self.declarations:
            if lines:
                lines.append('')
            lines += d.pretty_print()
        return lines


class Block(namedtuple('Block', ['block_items'])):
    def pretty_print(self):
        inner = [line for bi in self.block_items for line in bi.pretty_print()]
        return ['{'] + indent(inner) + ['}']


class BlockItem:
    def pretty_print(self):
        return [str(self)]


##
## Declarations
##


class Declaration(BlockItem):
    pass


class VarDeclaration(Declaration, namedtuple('Declaration', ['name', 'init', 'var_type', 'storage_class'])):
    def pretty_print(self):
        parts = [self.var_type.pretty_print(), self.name]
        if self.init:
            parts.append('=')
            parts.append(self.init.pretty_print())
        parts.append(str(self.storage_class))
        return [' '.join(parts)]


class FuncDeclaration(Declaration,
                      namedtuple('Declaration', ['name', 'params', 'body', 'fun_type', 'storage_class'])):
    def pretty_print(self):
        params = ', '.join(self.params)
        t = self.fun_type.pretty_print()
        header = f'function {self.name}({params}) :: {t}'
        if self.body:
            body = self.body.pretty_print()
            return headed(header, body)
        else:
            return [header + ';']


class StructDeclaration(Declaration, namedtuple('StructDeclaration',  ['tag', 'fields'])):
    def pretty_print(self):
        fields = [f.pretty_print() for f in fields]
        header = f'struct {self.tag}'
        if fields:
            return headed(header, fields)
        else:
            return [header + ';']


class StructField(namedtuple('StructField', ['name', 'type'])):
    def pretty_print(self):
        type = self.type.pretty_print()
        return f'{type} {self.name},'


class StorageClass:
    pass


class Static(StorageClass):
    def __eq__(self, other):
        return isinstance(other, Static)

    def __ne__(self, other):
        return not (self == other)

    def __str__(self):
        return 'Static()'


class Extern(StorageClass):
    def __eq__(self, other):
        return isinstance(other, Extern)

    def __ne__(self, other):
        return not (self == other)

    def __str__(self):
        return 'Extern'


##
## Statements
##

class Statement(BlockItem):
    pass


class Compound(Statement, namedtuple('Compound', ['block'])):
    ''' take a Block() as block '''
    def pretty_print(self):
        return self.block.pretty_print()


class Return(Statement, namedtuple('Return', ['expr'])):
    def pretty_print(self):
        if self.expr is None:
            return ['return']
        return [f'return {self.expr.pretty_print()}']


class ExprStmt(Statement, namedtuple('ExprStmt', ['expr'])):
    def pretty_print(self):
        return [self.expr.pretty_print()]


class NullStatement(Statement, namedtuple('NullStatement', [])):
    pass


class IfStatement(Statement, namedtuple('IfStatement', ['condition', 't', 'e'])):
    def pretty_print(self):
        header = f'if {self.condition.pretty_print()}'
        lines = headed(header, self.t.pretty_print())
        if self.e:
            else_lines = headed('else', self.e.pretty_print())
            lines = trailer(lines, else_lines[0]) + else_lines[1:]
        return lines


class Goto(Statement, namedtuple('Goto', ['label'])):
    pass


class LabeledStmt(Statement, namedtuple('LabeledExpr', ['label', 'stmt'])):
    def pretty_print(self):
        return [f'label {self.label}:'] + self.stmt.pretty_print()


class Break(Statement, namedtuple('Break', ['loop_label'])):
    ''' loop_label is added by the validator pass '''
    pass


class Continue(Statement, namedtuple('Continue', ['loop_label'])):
    ''' loop_label is added by the validator pass '''
    pass


class While(Statement, namedtuple('While', ['test', 'body', 'loop_label'])):
    def pretty_print(self):
        header = f'while {self.test.pretty_print()}'
        return headed(header, self.body.pretty_print())


class DoWhile(Statement, namedtuple('DoWhile', ['body', 'test', 'loop_label'])):
    def pretty_print(self):
        lines = headed('do', self.body.pretty_print())
        return trailer(lines, f'while {self.test.pretty_print()}')


class For(Statement, namedtuple('For', ['init', 'condition', 'post', 'body', 'loop_label'])):
    def pretty_print(self):
        init = ''
        if self.init:
            init = self.init.pretty_print()
        cond = ''
        if self.condition:
            cond = self.condition.pretty_print()
        post = ''
        if self.post:
            post = self.post.pretty_print()
        header = f'for ({init}; {cond}; {post})'
        return headed(header, self.body.pretty_print())


class Switch(Statement, namedtuple('Switch', ['condition', 'body', 'switch_label', 'case_values'])):
    def pretty_print(self):
        header = f'switch ({self.condition.pretty_print()})'
        return headed(header, self.body.pretty_print())


class Case(Statement, namedtuple('Case', ['value', 'stmt', 'switch_label'])):
    def pretty_print(self):
        lines = [f'case {self.value}: <#{self.switch_label}>']
        if self.stmt:
            lines += self.stmt.pretty_print()
        return lines


class Default(Statement, namedtuple('Default', ['stmt', 'switch_label'])):
    def pretty_print(self):
        lines = [f'default: <#{self.switch_label}>']
        if self.stmt:
            lines += self.stmt.pretty_print()
        return lines


##
## For initializers
##

class ForInit:
    pass


class InitDecl(ForInit, namedtuple('InitDecl', ['declaration'])):
    ''' declaration is a VarDeclaration '''
    def pretty_print(self):
        return ' '.join(self.declaration.pretty_print())


class InitExp(ForInit, namedtuple('InitExp', ['expression'])):
    def pretty_print(self):
        if not self.expression:
            return ''
        return self.expression.pretty_print()


##
## Expressions
##

class Expression:
    def __init__(self, *args, **kwargs):
        self.expr_type = kwargs.get('expr_type', None)

    def set_type(self, expr_type):
        assert(expr_type is not None)
        self.expr_type = expr_type
        return self

    def pretty_print(self):
        return f'<{self.expr_type}>{self}'


class Constant(Expression, namedtuple('Constant', ['const'])):
    pass


class String(Expression, namedtuple('String', ['bytes'])):
    pass


class Variable(Expression, namedtuple('Variable', ['name'])):
    pass


class Cast(Expression, namedtuple('Cast', ['target_type', 'expr'])):
    def __str__(self):
        return f'Cast({self.target_type.pretty_print()}, {self.expr.pretty_print()})'


class Unary(Expression, namedtuple('Unary', ['operator', 'expr'])):
    def __str__(self):
        return f'Unary({self.operator.pretty_print()}, {self.expr.pretty_print()})'


class Postfix(Expression, namedtuple('Postfix', ['expr', 'operator'])):
    def __str__(self):
        return f'Postfix({self.expr.pretty_print()}, {self.operator.pretty_print()})'


class Binary(Expression, namedtuple('Binary', ['operator', 'left', 'right'])):
    def __str__(self):
        return f'Binary({self.operator.pretty_print()}, {self.left.pretty_print()}, {self.right.pretty_print()})'


class Assignment(Expression, namedtuple('Assignment', ['lhs', 'rhs', 'op'])):
    def __str__(self):
        op = '='
        if self.op is not None:
            op = self.op.pretty_print()
        return f'Assignment({self.lhs.pretty_print()}, {self.rhs.pretty_print()}, {op})'


class Conditional(Expression, namedtuple('Conditional', ['condition', 't', 'e'])):
    def __str__(self):
        return f'Conditional({self.condition.pretty_print()}, {self.t.pretty_print()}, {self.e.pretty_print()})'


class Call(Expression, namedtuple('Call', ['function', 'arguments'])):
    def __str__(self):
        args = [a.pretty_print() for a in self.arguments]
        return f'Call({self.function}, {args})'


class Dereference(Expression, namedtuple('Dereference', ['inner'])):
    def __str__(self):
        inner = self.inner.pretty_print()
        return f'Dereference({inner})'


class AddrOf(Expression, namedtuple('AddrOf', ['inner'])):
    def __str__(self):
        inner = self.inner.pretty_print()
        return f'AddrOf({inner})'


class Sequence(Expression, namedtuple('Sequence', ['first', 'second'])):
    def __str__(self):
        first = self.first.pretty_print()
        second = self.second.pretty_print()
        return f'Sequence({first}, {second})'


class Subscript(Expression, namedtuple('Subscript', ['left', 'right'])):
    def __str__(self):
        left = self.left.pretty_print()
        right = self.right.pretty_print()
        return f'Subscript({left}, {right})'


class Dot(Expression, namedtuple('Dot', ['structure', 'member'])):
    def __str__(self):
        structure = self.structure.pretty_print()
        return f'Dot({structure}, {self.member})'


class Arrow(Expression, namedtuple('Arrow', ['pointer', 'member'])):
    def __str__(self):
        pointer = self.pointer.pretty_print()
        return f'Arrow({pointer}, {self.member})'


class SizeOf(Expression, namedtuple('SizeOf', ['expr'])):
    def __str__(self):
        expr = self.expr.pretty_print()
        return f'SizeOf({expr})'


class SizeOfT(Expression, namedtuple('SizeOfT', ['type'])):
    def __str__(self):
        type = self.type.pretty_print()
        return f'SizeOfT({type})'


##
## Array initializers
##

class Initializer:
    def __init__(self, *args, **kwargs):
        self.expr_type = None

    def pretty_print(self):
        return str(self)

    def set_type(self, expr_type):
        assert(expr_type is not None)
        self.expr_type = expr_type
        return self

    def pretty_print(self):
        return f'<{self.expr_type}>{self}'


class SingleInit(Initializer, namedtuple('SingleInit', ['expr'])):
    def __str__(self):
        expr = self.expr.pretty_print()
        return f'SingleInit({expr})'


class CompoundInit(Initializer, namedtuple('CompoundInit', ['initializers'])):
    def __str__(self):
        initializers = ', '.join([ini.pretty_print() for ini in self.initializers])
        return f'CompoundInit({initializers})'


##
## Unary ops
##

class UnaryOp:
    def __eq__(self, other):
        return type(self) == type(other)

    def __ne__(self, other):
        return not (self == other)

    def pretty_print(self):
        return self.__class__.__name__


class UnaryNegate(UnaryOp, namedtuple('UnaryNegate', [])):
    ''' -n '''
    pass


class UnaryInvert(UnaryOp, namedtuple('UnaryInvert', [])):
    ''' ~n '''
    pass


class UnaryNot(UnaryOp, namedtuple('UnaryNot', [])):
    ''' !n '''
    pass


class UnaryIncrement(UnaryOp, namedtuple('UnaryIncrement', [])):
    ''' ++n or n++ '''
    pass


class UnaryDecrement(UnaryOp, namedtuple('UnaryDecrement', [])):
    ''' --n or n-- '''
    pass

##
## Binary Ops
##


class BinaryOp:
    def __eq__(self, other):
        return type(self) == type(other)

    def __ne__(self, other):
        return not (self == other)

    def pretty_print(self):
        return self.__class__.__name__


class BinaryAdd(BinaryOp, namedtuple('BinaryAdd', [])):
    pass


class BinarySubtract(BinaryOp, namedtuple('BinarySubtract', [])):
    pass


class BinaryMultiply(BinaryOp, namedtuple('BinaryMultiply', [])):
    pass


class BinaryDivide(BinaryOp, namedtuple('BinaryDivide', [])):
    pass


class BinaryRemainder(BinaryOp, namedtuple('BinaryRemainder', [])):
    pass


class BitAnd(BinaryOp, namedtuple('BitAnd', [])):
    pass


class BitOr(BinaryOp, namedtuple('BitOr', [])):
    pass


class BitXor(BinaryOp, namedtuple('BitXor', [])):
    pass


class ShiftLeft(BinaryOp, namedtuple('ShiftLeft', [])):
    pass


class ShiftRight(BinaryOp, namedtuple('ShiftRight', [])):
    pass


class Less(BinaryOp, namedtuple('Less', [])):
    pass


class LessEqual(BinaryOp, namedtuple('LessEqual', [])):
    pass


class Greater(BinaryOp, namedtuple('Greater', [])):
    pass


class GreaterEqual(BinaryOp, namedtuple('GreaterEqual', [])):
    pass


class Equals(BinaryOp, namedtuple('Equals', [])):
    pass


class NotEquals(BinaryOp, namedtuple('NotEquals', [])):
    pass


class BinaryAnd(BinaryOp, namedtuple('BinaryAnd', [])):
    pass


class BinaryOr(BinaryOp, namedtuple('BinaryOr', [])):
    pass


##
## Constants
##

class Const:
    pass


class ConstInt(Const, namedtuple('ConstInt', ['value'])):
    def __key(self):
        return ('Int', int(self.value))

    def __hash__(self):
        return hash(self.__key())

    def pretty_print(self):
        return f'ConstInt({self.value})'


class ConstLong(Const, namedtuple('ConstLong', ['value'])):
    def __key(self):
        return ('Long', int(self.value))

    def __hash__(self):
        return hash(self.__key())

    def pretty_print(self):
        return f'ConstLong({self.value})'


class ConstUInt(Const, namedtuple('ConstUInt', ['value'])):
    def __key(self):
        return ('UInt', int(self.value))

    def __hash__(self):
        return hash(self.__key())

    def pretty_print(self):
        return f'ConstUInt({self.value})'


class ConstULong(Const, namedtuple('ConstULong', ['value'])):
    def __key(self):
        return ('ULong', int(self.value))

    def __hash__(self):
        return hash(self.__key())

    def pretty_print(self):
        return f'ConstULong({self.value})'


class ConstDouble(Const, namedtuple('ConstDouble', ['value'])):
    def __key(self):
        return ('Double', self.value)

    def __hash__(self):
        return hash(self.__key())

    def pretty_print(self):
        return f'ConstDouble({self.value})'


class ConstUChar(Const, namedtuple('ConstUChar', ['value'])):
    def __key(self):
        return ('UChar', self.value)

    def __hash__(self):
        return hash(self.__key())

    def pretty_print(self):
        return f'ConstUChar({self.value})'


class ConstChar(Const, namedtuple('ConstChar', ['value'])):
    def __key(self):
        return ('Char', self.value)

    def __hash__(self):
        return hash(self.__key())

    def pretty_print(self):
        return f'ConstChar({self.value})'
