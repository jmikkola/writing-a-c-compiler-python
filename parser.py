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
        return syntax.Function(name=name, body=body)

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

    def parse_expression(self) -> syntax.Expression:
        ''' parse an expression '''
        if self.peek('constant'):
            return self.parse_constant()
        else:
            self.fail('expected an expression')

    def parse_constant(self) -> syntax.Constant:
        ''' parse a constant (an integer) '''
        const = self.expect('constant')
        value = int(const.text)
        return syntax.Constant(value=value)

    def peek(self, kind, value=None):
        if not self.tokens:
            return None
        token = self.tokens[0]
        if token.kind != kind:
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
