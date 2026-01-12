from collections import namedtuple

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

    def __eq__(self, other):
        # Super lazy
        return repr(self) == repr(other)

    def __ne__(self, other):
        return not (self == other)


class Int(Type, namedtuple('Int', [])):
    pass


class Long(Type, namedtuple('Type', [])):
    pass


class Func(Type, namedtuple('Func', ['params', 'ret'])):
    pass


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
    pass


class FuncDeclaration(Declaration,
                      namedtuple('Declaration', ['name', 'params', 'body', 'fun_type', 'storage_class'])):
    def pretty_print(self):
        params = ', '.join(self.params)
        header = f'function {self.name}({params})'
        if self.body:
            body = self.body.pretty_print()
            return headed(header, body)
        else:
            return [header + ';']


class StorageClass:
    pass


class Static(StorageClass):
    def __eq__(self, other):
        return isinstance(other, Static)

    def __ne__(self, other):
        return not (self == other)


class Extern(StorageClass):
    def __eq__(self, other):
        return isinstance(other, Extern)

    def __ne__(self, other):
        return not (self == other)


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
    pass


class ExprStmt(Statement, namedtuple('ExprStmt', ['expr'])):
    pass


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
        lines = [f'case {self.loop_label}:']
        if self.stmt:
            lines += self.stmt.pretty_print()
        return lines


class Default(Statement, namedtuple('Default', ['stmt', 'switch_label'])):
    def pretty_print(self):
        lines = ['default:']
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
        return str(self)


class Constant(Expression, namedtuple('Constant', ['const'])):
    pass


class Variable(Expression, namedtuple('Variable', ['name'])):
    pass


class Cast(Expression, namedtuple('Cast', ['target_type', 'expr'])):
    pass


class Unary(Expression, namedtuple('Unary', ['operator', 'expr'])):
    pass


class Postfix(Expression, namedtuple('Postfix', ['expr', 'operator'])):
    pass


class Binary(Expression, namedtuple('Binary', ['operator', 'left', 'right'])):
    pass


class Assignment(Expression, namedtuple('Assignment', ['lhs', 'rhs', 'op'])):
    pass


class Conditional(Expression, namedtuple('Conditional', ['condition', 't', 'e'])):
    pass


class Call(Expression, namedtuple('Call', ['function', 'arguments'])):
    pass


##
## Unary ops
##

class UnaryOp:
    def __eq__(self, other):
        return type(self) == type(other)

    def __ne__(self, other):
        return not (self == other)


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
    pass


class ConstLong(Const, namedtuple('ConstLong', ['value'])):
    pass
