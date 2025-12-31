import syntax


def parse(tokens):
    p = Parser(tokens)
    return p.parse()


class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.parsed_tokens = []

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
        self.expect('{')
        body = self.parse_statement()
        self.expect('}')
        return syntax.Function(name=name.text, body=body)

    def parse_statement(self) -> syntax.Statement:
        ''' parse a single statement '''
        if self.peek('keyword', 'return'):
            return self.parse_return()
        else:
            self.fail('expected a statement')

    def parse_return(self) -> syntax.Return:
        ''' parse a return statement '''
        self.expect('keyword', 'return')
        expression = self.parse_expression()
        self.expect(';')
        return syntax.Return(expr=expression)

    def parse_expression(self, min_prec=0) -> syntax.Expression:
        ''' parse an expression '''
        operators = [
            '+', '-', '*', '/', '%',
            '&', '|', '^',
            '&&', '||',
            '<<', '>>',
            '<', '<=', '==', '!=', '>=', '>',
        ]
        left = self.parse_factor()
        while True:
            token = self.peek()
            if token.text not in operators:
                break
            token_precedence = self.precedence(token.text)
            if token_precedence < min_prec:
                break
            op = self.parse_binary_op()
            right = self.parse_expression(min_prec=token_precedence + 1)
            left = syntax.Binary(op, left, right)
        return left

    def precedence(self, text):
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
        if token.text == '+':
            return syntax.BinaryAdd()
        if token.text == '-':
            return syntax.BinarySubtract()
        if token.text == '*':
            return syntax.BinaryMultiply()
        if token.text == '/':
            return syntax.BinaryDivide()
        if token.text == '%':
            return syntax.BinaryRemainder()
        if token.text == '|':
            return syntax.BitOr()
        if token.text == '&':
            return syntax.BitAnd()
        if token.text == '^':
            return syntax.BitXor()
        if token.text == '<<':
            return syntax.ShiftLeft()
        if token.text == '>>':
            return syntax.ShiftRight()
        if token.text == '<':
            return syntax.Less()
        if token.text == '<=':
            return syntax.LessEqual()
        if token.text == '>':
            return syntax.Greater()
        if token.text == '>=':
            return syntax.GreaterEqual()
        if token.text == '==':
            return syntax.Equals()
        if token.text == '!=':
            return syntax.NotEquals()
        if token.text == '&&':
            return syntax.BinaryAnd()
        if token.text == '||':
            return syntax.BinaryOr()
        raise Exception(f'Unhandled binary op {token.text}')

    def parse_factor(self) -> syntax.Expression:
        ''' parse a factor (anything without a binary expression) '''
        if self.peek('constant'):
            return self.parse_constant()
        if self.peek('-') or self.peek('~') or self.peek('!'):
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

    def parse_unary_operator(self) -> syntax.UnaryOp:
        token = self.consume()
        if token.text == '-':
            return syntax.UnaryNegate()
        if token.text == '~':
            return syntax.UnaryInvert()
        if token.text == '!':
            return syntax.UnaryNot()
        raise Exception(f'unhandled unary operator {token.text}')

    def parse_constant(self) -> syntax.Constant:
        ''' parse a constant (an integer) '''
        const = self.expect('constant')
        value = int(const.text)
        return syntax.Constant(value=value)

    def peek(self, kind=None, value=None):
        if not self.tokens:
            return None
        token = self.tokens[0]
        if kind is not None and token.kind != kind:
            return None
        if value is not None and token.text != value:
            return None
        return token

    def expect(self, kind, value=None):
        token = self.peek(kind, value)
        if token is None:
            self.fail('syntax error', kind, value)
        self.tokens = self.tokens[1:]
        self.parsed_tokens.append(token)
        return token

    def consume(self):
        if not self.tokens:
            self.fail('unexpected EOF')
        token = self.tokens[0]
        self.tokens = self.tokens[1:]
        self.parsed_tokens.append(token)
        return token

    def must_eof(self):
        if self.tokens:
            self.fail('extra junk', 'EOF')

    def fail(self, message, expected=None, value=None):
        msg = 'Syntax error: ' + message
        if value:
            msg += f'\n  expected {value}'
        elif expected:
            msg += f'\n  expected a {expected}'

        if self.tokens:
            msg += f'\n  got {self.tokens[0].text}'

        recent = self.parsed_tokens[-5:]
        recent_text = ' '.join(r.text for r in recent)
        if recent_text:
            msg += f'\n  after {recent_text}'

        raise SyntaxError(msg)


class SyntaxError(Exception):
    pass
