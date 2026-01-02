import syntax
from errors import SyntaxError


def parse(tokens):
    p = Parser(tokens)
    return p.parse()


ASSIGNMENT_OPS = ['=', '>>=', '<<=', '+=', '-=', '*=', '/=', '%=', '&=', '|=', '^=']


class Parser:
    def __init__(self, tokens):
        self.token_iter = TokenIterator(tokens)

    def parse(self) -> syntax.Program:
        ''' parse a program '''
        function = self.parse_function()
        self.must_eof()
        return syntax.Program(function_definition=function)

    def parse_function(self) -> syntax.Function:
        ''' parse a function definition '''
        self.expect('keyword', 'int')
        name = self.expect('identifier')
        self.expect('(')
        self.expect('keyword', 'void')
        self.expect(')')
        body = self.parse_block()
        return syntax.Function(name=name.text, body=body)

    def parse_block(self) -> syntax.Block:
        self.expect('{')
        block_items = []
        while not self.peek('}'):
            block_items.append(self.parse_block_item())
        self.expect('}')
        return syntax.Block(block_items)

    def parse_block_item(self) -> syntax.BlockItem:
        if self.peek('keyword', 'int'):
            return self.parse_declaration()
        return self.parse_statement()

    def parse_declaration(self) -> syntax.Declaration:
        self.expect('keyword', 'int')
        name_token = self.expect('identifier')
        init = None
        if self.peek('='):
            self.expect('=')
            init = self.parse_expression()
        self.expect(';')
        return syntax.Declaration(name_token.text, init)

    def parse_statement(self) -> syntax.Statement:
        ''' parse a single statement '''
        if self.peek('keyword', 'return'):
            return self.parse_return()
        if self.peek('keyword', 'if'):
            return self.parse_if()
        if self.peek('keyword', 'goto'):
            return self.parse_goto()
        if self.peek('keyword', 'break'):
            self.consume()
            self.expect(';')
            return syntax.Break(None)
        if self.peek('keyword', 'continue'):
            self.consume()
            self.expect(';')
            return syntax.Continue(None)
        if self.peek('keyword', 'for'):
            return self.parse_for()
        if self.peek('keyword', 'while'):
            return self.parse_while()
        if self.peek('keyword', 'do'):
            return self.parse_do_while()
        if self.peek('keyword', 'switch'):
            return self.parse_switch()
        if self.peek('keyword', 'case'):
            return self.parse_case()
        if self.peek('keyword', 'default'):
            return self.parse_default()
        if self.peek('identifier') and self.peek(':', offset=1):
            return self.parse_labeled_stmt()
        if self.peek(';'):
            self.consume()
            return syntax.NullStatement()
        if self.peek('{'):
            block = self.parse_block()
            return syntax.Compound(block)
        return self.parse_expression_statement()

    def parse_while(self) -> syntax.While:
        self.expect('keyword', 'while')
        self.expect('(')
        test = self.parse_expression()
        self.expect(')')
        body = self.parse_statement()
        return syntax.While(test, body, None)

    def parse_do_while(self) -> syntax.DoWhile:
        self.expect('keyword', 'do')
        body = self.parse_statement()
        self.expect('keyword', 'while')
        self.expect('(')
        test = self.parse_expression()
        self.expect(')')
        self.expect(';')
        return syntax.DoWhile(body, test, None)

    def parse_for(self) -> syntax.For:
        self.expect('keyword', 'for')
        self.expect('(')
        #  Parsing the initializer eats the first ;
        init = self.parse_for_init()
        condition = None
        if not self.peek(';'):
            condition = self.parse_expression()
        self.expect(';')
        post = None
        if not self.peek(')'):
            post = self.parse_expression()
        self.expect(')')
        body = self.parse_statement()
        return syntax.For(init, condition, post, body, None)

    def parse_for_init(self) -> syntax.ForInit:
        if self.peek('keyword', 'int'):
            declaration = self.parse_declaration()
            return syntax.InitDecl(declaration)
        if self.peek(';'):
            self.expect(';')
            return syntax.InitExp(None)
        expression = self.parse_expression()
        self.expect(';')
        return syntax.InitExp(expression)

    def parse_switch(self) -> syntax.Switch:
        self.expect('keyword', 'switch')
        self.expect('(')
        condition = self.parse_expression()
        self.expect(')')
        body = self.parse_statement()
        return syntax.Switch(condition, body, None)

    def parse_case(self) -> syntax.Case:
        self.expect('keyword', 'case')
        value = self.parse_expression()
        self.expect(':')
        stmt = None
        if not self.peek('}'):
            stmt = self.parse_statement()
        return syntax.Case(value, stmt, None)

    def parse_default(self) -> syntax.Default:
        self.expect('keyword', 'default')
        self.expect(':')
        stmt = None
        if not self.peek('}'):
            stmt = self.parse_statement()
        return syntax.Default(stmt, None)

    def parse_labeled_stmt(self) -> syntax.LabeledStmt:
        label = self.expect('identifier').text
        self.expect(':')
        stmt = self.parse_statement()
        return syntax.LabeledStmt(label, stmt)

    def parse_expression_statement(self) -> syntax.ExprStmt:
        expr = self.parse_expression()
        self.expect(';')
        return syntax.ExprStmt(expr)

    def parse_return(self) -> syntax.Return:
        ''' parse a return statement '''
        self.expect('keyword', 'return')
        expression = self.parse_expression()
        self.expect(';')
        return syntax.Return(expr=expression)

    def parse_goto(self) -> syntax.Goto:
        self.expect('keyword', 'goto')
        label = self.expect('identifier').text
        self.expect(';')
        return syntax.Goto(label)

    def parse_if(self) -> syntax.IfStatement:
        self.expect('keyword', 'if')
        self.expect('(')
        condition = self.parse_expression()
        self.expect(')')
        t = self.parse_statement()
        e = None
        if self.peek('keyword', 'else'):
            self.expect('keyword', 'else')
            e = self.parse_statement()
        return syntax.IfStatement(condition, t, e)

    def parse_expression(self, min_prec=0) -> syntax.Expression:
        ''' parse an expression '''
        operators = [
            '+', '-', '*', '/', '%',
            '&', '|', '^',
            '&&', '||',
            '<<', '>>',
            '<', '<=', '==', '!=', '>=', '>',
            '+=', '-=', '*=', '/=', '%=',
            '&=', '|=', '^=',
            '>>=', '<<=',
            '=', '?',
        ]
        left = self.parse_factor()
        while True:
            token = self.peek()
            if token.text not in operators:
                break
            token_precedence = self.precedence(token.text)
            if token_precedence < min_prec:
                break

            # Handle = differently because it is right-associative.
            token = self.peek()
            if token and token.text in ASSIGNMENT_OPS:
                self.consume()
                right = self.parse_expression(min_prec=token_precedence)
                op = None
                if token.text != '=':
                    # Take off the trailing '=' and convert it to an operator
                    op = self.to_binary_op(token.text[:-1])
                return syntax.Assignment(lhs=left, rhs=right, op=op)
            elif self.peek('?'):
                self.expect('?')
                middle = self.parse_expression(min_prec=0)
                self.expect(':')
                right = self.parse_expression(min_prec=token_precedence)
                left = syntax.Conditional(left, middle, right)
            else:
                op = self.parse_binary_op()
                right = self.parse_expression(min_prec=token_precedence + 1)
                left = syntax.Binary(op, left, right)
        return left

    def precedence(self, text):
        if text in ASSIGNMENT_OPS:
            return 1
        if text == '?':
            return 3
        if text == '||':
            return 31
        if text == '&&':
            return 32
        if text == '|':
            return 33
        if text == '^':
            return 34
        if text == '&':
            return 35
        if text in ['==', '!=']:
            return 38
        if text in ['<', '<=', '>', '>=']:
            return 39
        if text in ['<<', '>>']:
            return 40
        if text in ['+', '-']:
            return 45
        if text in ['*', '%', '/']:
            return 50
        raise Exception(f'undefined precedence for {text}')

    def parse_binary_op(self) -> syntax.BinaryOp:
        token = self.consume()
        return self.to_binary_op(token.text)

    def to_binary_op(self, text):
        if text == '+':
            return syntax.BinaryAdd()
        if text == '-':
            return syntax.BinarySubtract()
        if text == '*':
            return syntax.BinaryMultiply()
        if text == '/':
            return syntax.BinaryDivide()
        if text == '%':
            return syntax.BinaryRemainder()
        if text == '|':
            return syntax.BitOr()
        if text == '&':
            return syntax.BitAnd()
        if text == '^':
            return syntax.BitXor()
        if text == '<<':
            return syntax.ShiftLeft()
        if text == '>>':
            return syntax.ShiftRight()
        if text == '<':
            return syntax.Less()
        if text == '<=':
            return syntax.LessEqual()
        if text == '>':
            return syntax.Greater()
        if text == '>=':
            return syntax.GreaterEqual()
        if text == '==':
            return syntax.Equals()
        if text == '!=':
            return syntax.NotEquals()
        if text == '&&':
            return syntax.BinaryAnd()
        if text == '||':
            return syntax.BinaryOr()
        raise Exception(f'Unhandled binary op {text}')

    def parse_factor(self) -> syntax.Expression:
        ''' parse a factor (anything without a binary expression) '''
        expr = self.parse_factor_without_postfix()
        while self.peek('++') or self.peek('--'):
            if self.peek('++'):
                self.expect('++')
                expr = syntax.Postfix(expr, syntax.UnaryIncrement())
            else:
                self.expect('--')
                expr = syntax.Postfix(expr, syntax.UnaryDecrement())
        return expr

    def parse_factor_without_postfix(self):
        if self.peek('constant'):
            return self.parse_constant()
        if self.peek('identifier'):
            token = self.consume()
            return syntax.Variable(token.text)
        if self.is_unary():
            return self.parse_unary_expression()
        if self.peek('('):
            return self.paren_expression()
        else:
            self.fail('expected an expression')

    def paren_expression(self) -> syntax.Expression:
        self.expect('(')
        expr = self.parse_expression()
        self.expect(')')
        return expr

    def parse_unary_expression(self) -> syntax.Unary:
        operator = self.parse_unary_operator()
        expr = self.parse_factor()
        return syntax.Unary(operator, expr)

    def is_unary(self):
        unary_operators = ['-', '~', '!', '++', '--']
        token = self.peek()
        return token and token.text in unary_operators

    def parse_unary_operator(self) -> syntax.UnaryOp:
        token = self.consume()
        if token.text == '-':
            return syntax.UnaryNegate()
        if token.text == '~':
            return syntax.UnaryInvert()
        if token.text == '!':
            return syntax.UnaryNot()
        if token.text == '++':
            return syntax.UnaryIncrement()
        if token.text == '--':
            return syntax.UnaryDecrement()
        raise Exception(f'unhandled unary operator {token.text}')

    def parse_constant(self) -> syntax.Constant:
        ''' parse a constant (an integer) '''
        const = self.expect('constant')
        value = int(const.text)
        return syntax.Constant(value=value)

    def peek(self, kind=None, value=None, offset=0):
        return self.token_iter.peek(kind, value, offset)

    def expect(self, kind, value=None):
        return self.token_iter.expect(self, kind, value)

    def consume(self):
        return self.token_iter.consume(self)

    def must_eof(self):
        return self.token_iter.must_eof(self)

    def fail(self, message, expected=None, value=None):
        msg = 'Syntax error: ' + message
        if value:
            msg += f'\n  expected {value}'
        elif expected:
            msg += f'\n  expected a {expected}'

        current = self.peek()
        if current:
            msg += f'\n  got {current.text}'

        recent = self.token_iter.recent()
        recent_text = ' '.join(r.text for r in recent)
        if recent_text:
            msg += f'\n  after {recent_text}'

        raise SyntaxError(msg)


class TokenIterator:
    def __init__(self, tokens):
        self.tokens = tokens
        self.next_token = 0

    def at_end(self, offset=0):
        return self.next_token + offset >= len(self.tokens)

    def _next(self):
        return self.tokens[self.next_token]

    def peek(self, kind=None, value=None, offset=0):
        if self.at_end(offset):
            return None
        token = self.tokens[self.next_token + offset]
        if kind is not None and token.kind != kind:
            return None
        if value is not None and token.text != value:
            return None
        return token

    def expect(self, parser, kind, value=None):
        token = self.peek(kind, value)
        if token is None:
            parser.fail('syntax error', kind, value)
        self.next_token += 1
        return token

    def consume(self, parser):
        if self.at_end():
            self.fail('unexpected EOF')
        token = self._next()
        self.next_token += 1
        return token

    def must_eof(self, parser):
        if not self.at_end():
            parser.fail('extra junk', 'EOF')

    def recent(self):
        start = max(0, self.next_token - 5)
        return self.tokens[start:self.next_token]
