import re
from typing import List

import syntax
from errors import SyntaxError


ASSIGNMENT_OPS = ['=', '>>=', '<<=', '+=', '-=', '*=', '/=', '%=', '&=', '|=', '^=']

TYPE_SPECIFIERS = ['int', 'long', 'signed', 'unsigned', 'double', 'char']
STORAGE_CLASSES = ['static', 'extern']

DIGITS = re.compile(r'\d+')
INT_TYPE_SPEC = re.compile(r'[lLuU]+')


def parse(tokens):
    p = Parser(tokens)
    return p.parse()


class Parser:
    def __init__(self, tokens):
        self.token_iter = TokenIterator(tokens)

    def parse(self) -> syntax.Program:
        ''' parse a program '''
        declarations = []
        while self.peek():
            declarations.append(self.parse_declaration())
        self.must_eof()
        return syntax.Program(declarations)

    def parse_function(self) -> syntax.FuncDeclaration:
        ''' parse a function definition '''
        base_type, storage_class = self.parse_storage_class_and_type()
        declarator = self.parse_declarator()
        (name, decl_type, arg_names) = self.process_declarator(declarator, base_type)
        if not self.is_function_type(decl_type):
            self.fail(f'declaration be a function: {name}')
        return self.parse_function_after_type(name, decl_type, arg_names, storage_class)

    def parse_function_after_type(self, name, decl_type, arg_names, storage_class):
        body = None
        if self.peek(';'):
            self.consume()
        else:
            body = self.parse_block()

        return syntax.FuncDeclaration(
            name=name,
            params=arg_names,
            body=body,
            fun_type=decl_type,
            storage_class=storage_class
        )

    def parse_type_specifier(self) -> syntax.Type:
        specifier_list = []
        while self.peek_type_specifier():
            specifier_list.append(self.expect('keyword').text)
        return self.type_from_specifiers(specifier_list)

    def is_type_specifier(self, text):
        return text in TYPE_SPECIFIERS

    def peek_type_specifier(self):
        token = self.peek('keyword')
        return token is not None and self.is_type_specifier(token.text)

    def type_from_specifiers(self, specifier_list) -> syntax.Type:
        if not specifier_list:
            self.fail('expected a type specifier')
        if len(specifier_list) != len(set(specifier_list)):
            self.fail(f'duplicates in the specifier list: {specifier_list}')
        if specifier_list == ['double']:
            return syntax.Double()
        if 'double' in specifier_list:
            self.fail(f'invalid specifier list: {specifier_list}')
        if 'signed' in specifier_list and 'unsigned' in specifier_list:
            self.fail('a type cannot have both signed and unsigned')

        if 'char' in specifier_list:
            for spec in specifier_list:
                if spec not in ['char', 'signed', 'unsigned']:
                    self.fail(f'a type cannot mix char and {spec}')
            if 'signed' in specifier_list:
                return syntax.SChar()
            if 'unsigned' in specifier_list:
                return syntax.UChar()
            assert(specifier_list == ['char'])
            return syntax.Char()

        is_unsigned = 'unsigned' in specifier_list
        is_long = 'long' in specifier_list

        if is_unsigned:
            if is_long:
                return syntax.ULong()
            else:
                return syntax.UInt()
        else:
            if is_long:
                return syntax.Long()
            else:
                return syntax.Int()

    def parse_declarator(self) -> syntax.Declarator:
        if self.peek('*'):
            self.expect('*')
            inner = self.parse_declarator()
            return syntax.PointerDeclarator(inner)
        else:
            return self.parse_direct_declarator()

    def parse_direct_declarator(self) -> syntax.Declarator:
        inner = self.parse_simple_declarator()
        if self.peek('('):
            param_list = self.parse_param_list()
            return syntax.FunDeclarator(param_list, inner)
        elif self.peek('['):
            while self.peek('['):
                self.expect('[')
                size = self.parse_integer_or_char()
                self.expect(']')
                inner = syntax.ArrayDeclarator(inner, size)
            return inner
        else:
            return inner

    def parse_simple_declarator(self) -> syntax.Declarator:
        if self.peek('('):
            self.expect('(')
            inner = self.parse_declarator()
            self.expect(')')
            return inner
        else:
            identifier = self.expect('identifier')
            return syntax.Ident(identifier.text)

    def parse_param_list(self) -> List[syntax.ParamInfo]:
        self.expect('(')
        if self.peek('keyword', 'void'):
            self.consume()
            self.expect(')')
            return []
        params = []
        while not self.peek(')'):
            if params:
                self.expect(',')
            t = self.parse_type_specifier()
            decl = self.parse_declarator()
            params.append(syntax.ParamInfo(t, decl))
        self.expect(')')
        return params

    def process_declarator(
            self,
            declarator: syntax.Declarator,
            base_type: syntax.Type
    ) -> (str, syntax.Type, list):
        match declarator:
            case syntax.Ident(identifier):
                return (identifier, base_type, [])
            case syntax.PointerDeclarator(d):
                derived_type = syntax.Pointer(base_type)
                return self.process_declarator(d, derived_type)
            case syntax.FunDeclarator(params, decl):
                match decl:
                    case syntax.Ident(identifier):
                        param_names = []
                        param_types = []
                        for p in params:
                            param_name, param_t, _ = self.process_declarator(p.declarator, p.t)
                            if isinstance(param_t, syntax.FunDeclarator):
                                self.fail('function pointers in parameters are not supported')
                            param_names.append(param_name)
                            param_types.append(param_t)

                        derived_type = syntax.Func(param_types, base_type)
                        return (identifier, derived_type, param_names)
                    case _:
                        self.fail('types derived from a function type are not supported')
            case syntax.ArrayDeclarator(element, size):
                derived_type = syntax.Array(base_type, size)
                return self.process_declarator(element, derived_type)

    def parse_optional_abstract_declarator(self):
        if self.peek('*') or self.peek('(') or self.peek('['):
            return self.parse_abstract_declarator()
        else:
            return syntax.AbstractBase()

    def parse_abstract_declarator(self) -> syntax.AbstractDeclarator:
        '''
        <abstract-declarator> := "*" [ <abstract-declarator> ]
                               | <direct-abstract-declarator>
        '''
        if self.peek('*'):
            self.consume()
            inner = self.parse_optional_abstract_declarator()
            return syntax.AbstractPointer(inner)
        else:
            return self.parse_direct_abstract_declarator()

    def parse_direct_abstract_declarator(self) -> syntax.AbstractDeclarator:
        '''
        <direct-abstract-declarator> := "(" <abstract-declarator> ")" { "[" <const> "]" }
                                      | { "[" <const> "]" } +
        '''
        if self.peek('('):
            self.expect('(')
            decl = self.parse_abstract_declarator()
            self.expect(')')
        else:
            decl = self.parse_array_abstract_declarator(syntax.AbstractBase())

        while self.peek('['):
            decl = self.parse_array_abstract_declarator(decl)
        return decl

    def parse_array_abstract_declarator(self, existing: syntax.AbstractDeclarator):
        self.expect('[')
        size = self.parse_integer()
        self.expect(']')
        return syntax.AbstractArray(existing, size)

    def process_abstract_declarator(
            self,
            base_type: syntax.Type,
            declarator: syntax.AbstractDeclarator
    ) -> syntax.Type:
        match declarator:
            case syntax.AbstractBase():
                return base_type
            case syntax.AbstractPointer(inner):
                derived_type = syntax.Pointer(base_type)
                return self.process_abstract_declarator(derived_type, inner)
            case syntax.AbstractArray(element, size):
                derived_type = syntax.Array(base_type, size)
                return self.process_abstract_declarator(derived_type, element)
            case _:
                raise Exception(f'unhandled abstract declarator {declarator}')

    def parse_declaration(self) -> syntax.Declaration:
        base_type, storage_class = self.parse_storage_class_and_type()
        declarator = self.parse_declarator()
        (name, decl_type, arg_names) = self.process_declarator(declarator, base_type)

        if self.is_function_type(decl_type):
            return self.parse_function_after_type(name, decl_type, arg_names, storage_class)
        else:
            return self.parse_var_declaration_after_type(name, decl_type, storage_class)

    def is_function_type(self, t: syntax.Type) -> bool:
        match t:
            case syntax.Func():
                return True
            case syntax.Pointer(inner):
                return self.is_function_type(inner)
            case _:
                return False

    def parse_var_declaration(self) -> syntax.VarDeclaration:
        base_type, storage_class = self.parse_storage_class_and_type()
        declarator = self.parse_declarator()
        (name, decl_type, arg_names) = self.process_declarator(declarator, base_type)
        if self.is_function_type(decl_type):
            self.fail(f'declaration must not be a function: {name}')
        return self.parse_var_declaration_after_type(name, decl_type, storage_class)

    def parse_var_declaration_after_type(self, name, decl_type, storage_class):
        init = None
        if self.peek('='):
            self.expect('=')
            init = self.parse_initializer()
        self.expect(';')
        return syntax.VarDeclaration(name, init, decl_type, storage_class)

    def parse_storage_class_and_type(self):
        storage_classes = []
        type_specifiers = []

        while True:
            token = self.peek('keyword')
            if not token:
                break

            if self.is_type_specifier(token.text):
                self.consume()
                type_specifiers.append(token.text)
            elif token.text in STORAGE_CLASSES:
                self.consume()
                storage_classes.append(token.text)
            else:
                self.fail(f'expected a type, "static", or "extern", got "{token.text}"')

        type_spec = self.type_from_specifiers(type_specifiers)
        storage_class = self.storage_from_classes(storage_classes)

        return type_spec, storage_class

    def storage_from_classes(self, storage_classes):
        if not storage_classes:
            return None
        elif storage_classes == ['static']:
            return syntax.Static()
        elif storage_classes == ['extern']:
            return syntax.Extern()
        else:
            self.fail(f'unexpected storage classes {storage_classes}')

    def parse_block(self) -> syntax.Block:
        self.expect('{')
        block_items = []
        while not self.peek('}'):
            block_items.append(self.parse_block_item())
        self.expect('}')
        return syntax.Block(block_items)

    def parse_block_item(self) -> syntax.BlockItem:
        if self.peek_declaration_keyword():
            return self.parse_declaration()
        return self.parse_statement()

    def peek_declaration_keyword(self):
        token = self.peek('keyword')
        return token is not None and self.is_declaration_keyword(token.text)

    def is_declaration_keyword(self, text):
        return text in TYPE_SPECIFIERS or text in STORAGE_CLASSES

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
        if self.peek_declaration_keyword():
            declaration = self.parse_var_declaration()
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
        return syntax.Switch(condition, body, None, None)

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

    def parse_argument_list(self):
        self.expect('(')
        arguments = []
        while not self.peek(')'):
            if arguments:
                self.expect(',')
            arguments.append(self.parse_expression())
        self.expect(')')
        return arguments

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
        while self.peek('++') or self.peek('--') or self.peek('['):
            if self.peek('++'):
                self.expect('++')
                expr = syntax.Postfix(expr, syntax.UnaryIncrement())
            elif self.peek('--'):
                self.expect('--')
                expr = syntax.Postfix(expr, syntax.UnaryDecrement())
            else:
                self.expect('[')
                subscript = self.parse_expression()
                self.expect(']')
                expr = syntax.Subscript(expr, subscript)
        return expr

    def parse_factor_without_postfix(self):
        if self.peek('constant'):
            return self.parse_constant()
        if self.peek('identifier'):
            token = self.consume()
            if self.peek('('):
                argument_list = self.parse_argument_list()
                return syntax.Call(token.text, argument_list)
            return syntax.Variable(token.text)
        if self.is_unary():
            return self.parse_unary_expression()
        if self.peek('('):
            # Hacky way to detect a cast:
            if self.peek('keyword', offset=1):
                return self.parse_cast()
            else:
                return self.paren_expression()
        else:
            self.fail('expected an expression')

    def parse_cast(self) -> syntax.Expression:
        self.expect('(')
        target_type = self.parse_type_specifier()
        declarator = self.parse_optional_abstract_declarator()
        derived_type = self.process_abstract_declarator(target_type, declarator)
        self.expect(')')
        expr = self.parse_factor()
        return syntax.Cast(derived_type, expr)

    def paren_expression(self) -> syntax.Expression:
        self.expect('(')
        expr = self.parse_expression()
        self.expect(')')
        return expr

    def parse_unary_expression(self) -> syntax.Unary:
        if self.peek('*'):
            return self.parse_dereference()
        if self.peek('&'):
            return self.parse_addr_of()
        operator = self.parse_unary_operator()
        expr = self.parse_factor()
        return syntax.Unary(operator, expr)

    def is_unary(self):
        unary_operators = ['-', '~', '!', '++', '--', '*', '&']
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

    def parse_dereference(self) -> syntax.Dereference:
        self.expect('*')
        expr = self.parse_factor()
        return syntax.Dereference(expr)

    def parse_addr_of(self) -> syntax.AddrOf:
        self.expect('&')
        expr = self.parse_factor()
        return syntax.AddrOf(expr)

    def parse_constant(self) -> syntax.Constant:
        ''' parse a constant (an integer or a double) '''
        token = self.expect('constant')
        text = token.text

        if text.startswith("'"):
            return self.parse_char_constant(text)

        if text.startswith('"'):
            return self.parse_string_constants(text)

        if '.' in text or 'e' in text or 'E' in text:
            # based on sys.float_info, it seems like python uses 64-bit floats
            return syntax.Constant(syntax.ConstDouble(float(text)))

        return self.parse_integer_constant_from_token(token)

    def parse_string_constants(self, text):
        ''' multiple string constants next to each other get concatenated '''
        string = self.parse_string_constant(text)

        while True:
            next = self.peek('constant')
            if next is None:
                break
            if not next.text.startswith('"'):
                break
            token = self.expect('constant')
            next_string = self.parse_string_constant(token.text)
            string = syntax.String(string.bytes + next_string.bytes)

        return string

    def parse_string_constant(self, text):
        assert(text[0] == '"')
        assert(text[-1] == '"')
        inner = text[1:-1]

        bytes = []
        i = 0
        while i < len(inner):
            if inner[i] == '\\':
                byte = self.escape_char(inner[i + 1])
                i += 2
            else:
                byte = ord(inner[i])
                i += 1
            bytes.append(byte)

        return syntax.String(bytes)

    def parse_char_constant(self, text):
        assert(text[0] == "'")
        assert(text[-1] == "'")
        assert(len(text) in (3,4))
        if len(text) == 3:
            c = text[1]
            value = ord(c)
        else:
            assert(text[1] == '\\')
            escaped = text[2]
            value = self.escape_char(escaped)
        return syntax.Constant(syntax.ConstInt(value))

    def escape_char(self, escaped):
        map = {
            "'": 39,
            '"': 34,
            '?': 63,
            '\\': 92,
            'a': 7,
            'b': 8,
            'f': 12,
            'n': 10,
            'r': 13,
            't': 9,
            'v': 11
        }
        # The lexer should ensure this
        assert(escaped in map)
        return map[escaped]

    def parse_integer_or_char(self) -> int:
        token = self.peek('constant')
        if token is None:
            self.fail(f'expected an integer or char')
        if token.text.startswith("'"):
            return self.parse_char()
        else:
            return self.parse_integer()

    def parse_char(self) -> int:
        token = self.expect('constant')
        text = token.text
        assert(text[0] == "'")
        assert(text[-1] == "'")
        assert(len(text) in (3,4))
        if len(text) == 3:
            c = text[1]
            return ord(c)
        else:
            assert(text[1] == '\\')
            escaped = text[2]
            return self.escape_char(escaped)

    def parse_integer(self) -> int:
        token = self.expect('constant')
        text = token.text
        if '.' in text or 'e' in text or 'E' in text:
            self.fail(f'invalid integer constant: {text}')
        digits = DIGITS.match(text).group()
        return int(digits)

    def parse_integer_constant_from_token(self, token) -> syntax.Constant:
        text = token.text

        digits = DIGITS.match(text).group()
        value = int(digits)

        is_long = False
        is_unsigned = False
        spec_match = INT_TYPE_SPEC.search(text)
        if spec_match is not None:
            spec = spec_match.group()
            is_long = 'l' in spec or 'L' in spec
            is_unsigned = 'u' in spec or 'U' in spec

        if is_unsigned:
            if value > (2**32 - 1):
                is_long = True
                if value > (2**64 - 1):
                    self.fail(f'constant is too large to represent as an unsigned long: {token.text}')

            if is_long:
                return syntax.Constant(syntax.ConstULong(value))
            else:
                return syntax.Constant(syntax.ConstUInt(value))
        else:
            if value > (2**31 - 1):
                is_long = True
                if value > (2**63 - 1):
                    self.fail(f'constant is too large to represent as a long: {token.text}')

            if is_long:
                return syntax.Constant(syntax.ConstLong(value))
            else:
                return syntax.Constant(syntax.ConstInt(value))

    def parse_initializer(self) -> syntax.Initializer:
        '''
        <initializer> ::= <exp>
                        | "{" <initializer> { "," <initializer> } [","] "}"
        '''
        if self.peek("{"):
            initializers = []
            self.expect("{")
            initializers.append(self.parse_initializer())
            while not self.peek("}"):
                self.expect(",")
                if self.peek("}"):
                    break
                initializers.append(self.parse_initializer())
            self.expect("}")
            return syntax.CompoundInit(initializers)
        else:
            expr = self.parse_expression()
            return syntax.SingleInit(expr)

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
            parser.fail('unexpected EOF')
        token = self._next()
        self.next_token += 1
        return token

    def must_eof(self, parser):
        if not self.at_end():
            parser.fail('extra junk', 'EOF')

    def recent(self):
        start = max(0, self.next_token - 5)
        return self.tokens[start:self.next_token]
