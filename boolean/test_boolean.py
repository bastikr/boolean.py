"""
Boolean Algebra.

Tests

Copyright (c) 2009-2010 Sebastian Kraemer, basti.kr@gmail.com
Released under revised BSD license.
"""

from __future__ import absolute_import, unicode_literals

import unittest
from unittest.case import expectedFailure

try:
    basestring  # Python 2
except NameError:
    basestring = str  # Python 3

import boolean


class ExpressionTestCase(unittest.TestCase):

    def test_creation(self):
        E = boolean.Expression()
        expr_str = '(a+b+c)*d*(~e+(f*g))'
        expr = E.parse(expr_str, simplify=False)
        self.assertEqual(expr_str, str(expr))

    def test_parse_with_mixed_operators_multilines_and_custom_symbol(self):

        class MySymbol(boolean.Symbol):
            pass

        expr_str = '''(a or ~ b +_c  ) and
                      d & ( ! e_
                      | (my * g OR 1 or 0) ) AND that '''

        E = boolean.Expression(symbol_class=MySymbol)
        expr = E.parse(expr_str, simplify=False)

        expected = boolean.AND(
            boolean.OR(
                MySymbol('a'),
                boolean.NOT(boolean.Symbol('b')),
                MySymbol('_c'),
            ),
            MySymbol('d'),
            boolean.OR(
                boolean.NOT(MySymbol('e_')),
                boolean.OR(
                    boolean.AND(
                        MySymbol('my'),
                        MySymbol('g'),
                    ),
                    boolean.TRUE(),
                    boolean.FALSE(),
                ),
            ),
            MySymbol('that'),
        )

        self.assertEqual(expected, expr)

    def test_parse_recognizes_trueish_and_falsish_symbol_tokens(self):
        expr_str = 'True or False or None or 0 or 1 or TRue or FalSE or NONe'
        E = boolean.Expression()
        expr = E.parse(expr_str, simplify=False)
        expected = boolean.OR(
            boolean.TRUE(),
            boolean.FALSE(),
            boolean.FALSE(),
            boolean.FALSE(),
            boolean.TRUE(),
            boolean.TRUE(),
            boolean.FALSE(),
            boolean.FALSE(),
        )
        self.assertEqual(expected, expr)

    def test_parse_can_use_iterable_from_alternative_tokenizer(self):

        class CustomSymbol(boolean.Symbol):
            pass

        def tokenizer(s):
            "Sample tokenizer using custom operators and symbols"
            ops = {
                'WHY_NOT': boolean.TOKEN_OR,
                'ALSO': boolean.TOKEN_AND,
                'NEITHER': boolean.TOKEN_NOT,
                '(': boolean.TOKEN_LPAR,
                ')': boolean.TOKEN_RPAR,
            }

            for row, line in enumerate(s.splitlines(False)):
                for col, tok in enumerate(line.split()):
                    if tok in ops:
                        yield ops[tok], tok, (row, col)
                    elif tok == 'Custom':
                        yield CustomSymbol(tok), tok, (row, col)
                    else:
                        yield boolean.TOKEN_SYMBOL, tok, (row, col)

        expr_str = '''( Custom WHY_NOT regular ) ALSO NEITHER  (
                      not_custom ALSO standard )
                   '''

        E = boolean.Expression(tokenizer_fun=tokenizer)
        expr = E.parse(expr_str, simplify=False)
        expected = boolean.AND(
            boolean.OR(
                CustomSymbol('Custom'),
                boolean.Symbol('regular'),
            ),
            boolean.NOT(
                boolean.AND(
                    boolean.Symbol('not_custom'),
                    boolean.Symbol('standard'),
                ),
            ),
        )
        self.assertEqual(expected, expr)

    def test_parse_with_advanced_tokenizer_example(self):
        import tokenize

        try:
            from io import StringIO
        except ImportError:
            try:
                from cStringIO import StringIO
            except ImportError:
                from StringIO import StringIO


        class PlainVar(boolean.Symbol):
            "Plain boolean variable"

        class ColonDotVar(boolean.Symbol):
            "Colon and dot-separated string boolean variable"

        def advanced_tokenizer_example(expr):
            """
            Example custom tokenizer derived from the standard Python tokenizer
            with a few extra features: #-style comments are supported and a
            colon- and dot-separated string is recognized and stored in custom
            symbols. In contrast with the standard tokenizer, only these
            boolean operators are recognized : & | ! and or not.

            For more advanced tokenization you could also consider forking the
            `tokenize` standard library module.
            """

            if not isinstance(expr, basestring):
                raise TypeError('expr must be string but it is %s.' % type(expr))

            # mapping of lowercase token strings to a token object instance for
            # standard operators, parens and common true or false symbols
            TOKENS = {
                '&': boolean.TOKEN_AND,
                'and': boolean.TOKEN_AND,
                '|': boolean.TOKEN_OR,
                'or': boolean.TOKEN_OR,
                '!': boolean.TOKEN_NOT,
                'not': boolean.TOKEN_NOT,
                '(': boolean.TOKEN_LPAR,
                ')': boolean.TOKEN_RPAR,
                'true': boolean.TOKEN_TRUE,
                '1': boolean.TOKEN_TRUE,
                'false': boolean.TOKEN_FALSE,
                '0': boolean.TOKEN_FALSE,
                'none': boolean.TOKEN_FALSE,
            }

            ignored_token_types = (
                tokenize.NL, tokenize.NEWLINE, tokenize.COMMENT,
                tokenize.INDENT, tokenize.DEDENT,
                tokenize.ENDMARKER
            )

            # note: an unbalanced expression may raise a TokenError here.
            tokens = ((toktype, tok, row, col,) for toktype, tok, (row, col,), _, _
                      in tokenize.generate_tokens(StringIO(expr).readline)
                      if tok and tok.strip())

            COLON_DOT = (':', '.',)

            def build_symbol(current_dotted):
                if current_dotted:
                    if any(s in current_dotted for s in COLON_DOT):
                        sym = ColonDotVar(current_dotted)
                    else:
                        sym = PlainVar(current_dotted)
                    return sym

            # accumulator for dotted symbols that span several `tokenize` tokens
            dotted, srow, scol = '', None, None

            for toktype, tok, row, col in tokens:
                if toktype in ignored_token_types:
                    # we reached a break point and should yield the current dotted
                    symbol = build_symbol(dotted)
                    if symbol is not None:
                        yield symbol, dotted, (srow, scol)
                        dotted, srow, scol = '', None, None

                    continue

                std_token = TOKENS.get(tok.lower())
                if std_token is not None:
                    # we reached a break point and should yield the current dotted
                    symbol = build_symbol(dotted)
                    if symbol is not None:
                        yield symbol, dotted, (srow, scol)
                        dotted, srow, scol = '', 0, 0

                    yield std_token, tok, (row, col)

                    continue

                if toktype == tokenize.NAME or (toktype == tokenize.OP and tok in COLON_DOT):
                    if not dotted:
                        srow = row
                        scol = col
                    dotted += tok

                else:
                    raise TypeError('Unknown token: %(tok)r at line: %(row)r, column: %(col)r' % locals())

        test_expr = '''
            (colon1:dot1.dot2 or colon2_name:col_on3:do_t1.do_t2.do_t3 )
            and
            ( plain_symbol & !Custom )
        '''

        E = boolean.Expression(tokenizer_fun=advanced_tokenizer_example)
        expr = E.parse(test_expr, simplify=False)
        expected = boolean.AND(
            boolean.OR(
                ColonDotVar('colon1:dot1.dot2'),
                ColonDotVar('colon2_name:col_on3:do_t1.do_t2.do_t3')
            ),
            boolean.AND(
                PlainVar('plain_symbol'),
                boolean.NOT(PlainVar('Custom'))
            )
        )
        self.assertEqual(expected, expr)


class BaseElementTestCase(unittest.TestCase):

    def test_creation(self):
        from boolean.boolean import BaseElement
        self.assertEqual(boolean.TRUE(), boolean.TRUE())
        BaseElement()
        self.assertRaises(TypeError, BaseElement, 2)
        self.assertRaises(TypeError, BaseElement, 'a')

    def test_literals(self):
        self.assertEqual(boolean.TRUE().literals, set())
        self.assertEqual(boolean.FALSE().literals, set())

    def test_literalize(self):
        self.assertEqual(boolean.TRUE().literalize(), boolean.TRUE())
        self.assertEqual(boolean.FALSE().literalize(), boolean.FALSE())

    def test_simplify(self):
        self.assertEqual(boolean.TRUE().simplify(), boolean.TRUE())
        self.assertEqual(boolean.FALSE().simplify(), boolean.FALSE())

    def test_dual(self):
        self.assertEqual(boolean.TRUE().dual, boolean.FALSE())
        self.assertEqual(boolean.FALSE().dual, boolean.TRUE())

    def test_equality(self):
        self.assertEqual(boolean.TRUE(), boolean.TRUE())
        self.assertEqual(boolean.FALSE(), boolean.FALSE())
        self.assertNotEqual(boolean.TRUE(), boolean.FALSE())

    def test_order(self):
        self.assertTrue(boolean.FALSE() < boolean.TRUE())
        self.assertTrue(boolean.TRUE() > boolean.FALSE())

    def test_printing(self):
        self.assertEqual(str(boolean.TRUE()), '1')
        self.assertEqual(str(boolean.FALSE()), '0')
        self.assertEqual(repr(boolean.TRUE()), 'TRUE()')
        self.assertEqual(repr(boolean.FALSE()), 'FALSE()')


class SymbolTestCase(unittest.TestCase):

    def test_init(self):
        boolean.Symbol(1)
        boolean.Symbol('a')
        boolean.Symbol(None)
        boolean.Symbol(sum)
        boolean.Symbol((1, 2, 3))
        boolean.Symbol([1, 2])

    def test_isliteral(self):
        self.assertTrue(boolean.Symbol(1).isliteral is True)

    def test_literals(self):
        l1 = boolean.Symbol(1)
        l2 = boolean.Symbol(1)
        self.assertTrue(l1 in l1.literals)
        self.assertTrue(l1 in l2.literals)
        self.assertTrue(l2 in l1.literals)
        self.assertTrue(l2 in l2.literals)
        self.assertRaises(AttributeError, setattr, l1, 'literals', 1)

    def test_literalize(self):
        s = boolean.Symbol(1)
        self.assertTrue(s.literalize() == s)

    def test_simplify(self):
        s = boolean.Symbol(1)
        self.assertTrue(s.simplify() == s)

    def test_equal(self):
        a = boolean.Symbol('a')
        b = boolean.Symbol('a')
        c = boolean.Symbol('b')
        d = boolean.Symbol('d')
        e = boolean.Symbol('e')
        # Test __eq__.
        self.assertEqual(a, a)
        self.assertEqual(a, b)
        self.assertNotEqual(a, c)
        self.assertNotEqual(b, c)
        self.assertEqual(d, d)
        self.assertNotEqual(d, e)
        self.assertNotEqual(a, d)
        # Test __ne__.
        self.assertFalse(a != a)
        self.assertFalse(a != b)
        self.assertTrue(a != c)
        self.assertTrue(b != c)

    def test_order(self):
        S = boolean.Symbol
        self.assertTrue(S('x') < S('y'))
        self.assertTrue(S('y') > S('x'))
        self.assertTrue(S(1) < S(2))
        self.assertTrue(S(2) > S(1))

    def test_printing(self):
        self.assertEqual('a', str(boolean.Symbol('a')))
        self.assertEqual('1', str(boolean.Symbol(1)))
        self.assertEqual("Symbol('a')", repr(boolean.Symbol('a')))
        self.assertEqual('Symbol(1)', repr(boolean.Symbol(1)))


class NOTTestCase(unittest.TestCase):

    def test_init(self):
        NOT = boolean.NOT
        self.assertRaises(TypeError, NOT)
        self.assertRaises(TypeError, NOT, 'a', 'b')
        NOT(boolean.Symbol('a'))
        self.assertEqual(boolean.FALSE(), (NOT(boolean.TRUE())).simplify())
        self.assertEqual(boolean.TRUE(), (NOT(boolean.FALSE())).simplify())

    def test_isliteral(self):
        s = boolean.Symbol(1)
        self.assertTrue(boolean.NOT(s).isliteral)
        self.assertFalse(boolean.Expression().parse('~(a+b)').isliteral)

    def test_literals(self):
        a = boolean.Symbol('a')
        l = ~a
        self.assertTrue(l.isliteral)
        self.assertTrue(l in l.literals)
        self.assertEqual(len(l.literals), 1)

        parse = boolean.Expression().parse
        l = parse('~(a*a)', simplify=False)
        self.assertFalse(l.isliteral)
        self.assertTrue(a in l.literals)
        self.assertEqual(len(l.literals), 1)

        l = parse('~(a*a)', simplify=True)
        self.assertTrue(l.isliteral)

    def test_literalize(self):
        parse = boolean.Expression().parse
        self.assertEqual(parse('~a').literalize(), parse('~a'))
        self.assertEqual(parse('~(a*b)').literalize(), parse('~a+~b'))
        self.assertEqual(parse('~(a+b)').literalize(), parse('~a*~b'))

    def test_simplify(self):
        a = boolean.Symbol('a')
        parse = boolean.Expression().parse
        self.assertEqual(~a, ~a)
        assert boolean.Symbol('a') == boolean.Symbol('a')
        self.assertNotEqual(a, parse('~~a', simplify=False))
        self.assertEqual(a, (~~a).simplify())
        self.assertEqual(~a, (~~~a).simplify())
        self.assertEqual(a, (~~~~a).simplify())
        self.assertEqual((~(a * a * a)).simplify(), (~(a * a * a)).simplify())
        self.assertEqual(a, parse('~~a', simplify=True))

    def test_cancel(self):
        a = boolean.Symbol('a')
        parse = boolean.Expression().parse
        self.assertEqual(~a, (~a).cancel())
        self.assertEqual(a, parse('~~a', simplify=False).cancel())
        self.assertEqual(~a, parse('~~~a', simplify=False).cancel())
        self.assertEqual(a, parse('~~~~a', simplify=False).cancel())

    def test_demorgan(self):
        a, b = boolean.Symbol('a'), boolean.Symbol('b')
        parse = boolean.Expression().parse
        self.assertEqual(parse('~(a*b)', simplify=False).demorgan(), ~a + ~b)
        self.assertEqual(parse('~(a+b+c)', simplify=False).demorgan(), parse('~a*~b*~c'))
        self.assertEqual(parse('~(~a*b)', simplify=False).demorgan(), a + ~b)

    def test_order(self):
        x = boolean.Symbol(1)
        y = boolean.Symbol(2)
        self.assertTrue(x < ~x)
        self.assertTrue(~x > x)
        self.assertTrue(~x < y)
        self.assertTrue(y > ~x)

    def test_printing(self):
        a = boolean.Symbol('a')
        self.assertEqual(str(~a), '~a')
        self.assertEqual(repr(~a), "NOT(Symbol('a'))")
        expr = boolean.Expression().parse('~(a*a)', simplify=False)
        self.assertEqual(str(expr), '~(a*a)')
        self.assertEqual(repr(expr), "NOT(AND(Symbol('a'), Symbol('a')))")


class DualBaseTestCase(unittest.TestCase):

    maxDiff = None

    def setUp(self):
        from boolean.boolean import DualBase

        self.a, self.b, self.c = boolean.Symbol('a'), boolean.Symbol('b'), boolean.Symbol('c')
        self.t1 = DualBase(self.a, self.b)
        self.t2 = DualBase(self.a, self.b, self.c)
        self.t3 = DualBase(self.a, self.a)
        self.t4 = DualBase(self.a, self.b, self.c)

    def test_init(self):
        from boolean.boolean import DualBase
        self.assertRaises(TypeError, DualBase)
        for term in (self.t1, self.t2, self.t3, self.t4):
            self.assertTrue(isinstance(term, DualBase))

    def test_isliteral(self):
        self.assertFalse(self.t1.isliteral)
        self.assertFalse(self.t2.isliteral)

    def test_literals(self):
        for term in (self.t1, self.t2, self.t3, self.t4):
            self.assertTrue(self.a in term.literals)
        for term in (self.t1, self.t2, self.t4):
            self.assertTrue(self.b in term.literals)
        for term in (self.t2, self.t4):
            self.assertTrue(self.c in term.literals)

    def test_literalize(self):
        parse = boolean.Expression().parse
        self.assertEqual(parse('a+~(b+c)', simplify=False).literalize(), parse('a+(~b*~c)', simplify=False))

    def test_annihilator(self):
        parse = boolean.Expression().parse
        self.assertEqual(parse('a*a', simplify=False).annihilator, boolean.FALSE())
        self.assertEqual(parse('a+a', simplify=False).annihilator, boolean.TRUE())

    def test_identity(self):
        parse = boolean.Expression().parse
        self.assertEqual(parse('a+b').identity, boolean.FALSE())
        self.assertEqual(parse('a*b').identity, boolean.TRUE())

    def test_dual(self):
        self.assertEqual(boolean.AND(boolean.Symbol('a'), boolean.Symbol('b')).dual, boolean.OR)
        self.assertEqual(boolean.OR(boolean.Symbol('a'), boolean.Symbol('b')).dual, boolean.AND)

        parse = boolean.Expression().parse
        self.assertEqual(parse('a+b').dual, boolean.AND)
        self.assertEqual(parse('a*b').dual, boolean.OR)

    def test_simplify(self):
        a = self.a
        b = self.b
        c = self.c
        _0 = boolean.FALSE()
        _1 = boolean.TRUE()
        # Idempotence
        self.assertEqual(a, (a * a).simplify())
        # Idempotence + Associativity
        self.assertEqual(a + b, (a + (a + b)).simplify())
        # Annihilation
        self.assertEqual(_0, (a * _0).simplify())
        self.assertEqual(_1, (a + _1).simplify())
        # Identity
        self.assertEqual(a, (a * _1).simplify())
        self.assertEqual(a, (a + _0).simplify())
        # Complementation
        self.assertEqual(_0, (a * ~a).simplify())
        self.assertEqual(_1, (a + ~a).simplify())
        # Absorption
        self.assertEqual(a, (a * (a + b)).simplify())
        self.assertEqual(a, (a + (a * b)).simplify())
        self.assertEqual(b * a, ((b * a) + (b * a * c)).simplify())

        # Elimination
        self.assertEqual(a, ((a * ~b) + (a * b)).simplify())

        parse = boolean.Expression().parse
        expected = parse('(a*b)+(b*c)+(a*c)', simplify=False)
        result = parse('(~a*b*c) + (a*~b*c) + (a*b*~c) + (a*b*c)', simplify=True)
        self.assertEqual(expected, result)

        expected = parse('b*d', simplify=False)
        result = parse('(a*b*c*d) + (b*d)', simplify=True)
        self.assertEqual(expected, result)

        expected = parse('(~b*~d*a) + (~c*~d*b) + (a*c*d)', simplify=True)
        result = parse('''(~a*b*~c*~d) + (a*~b*~c*~d) + (a*~b*c*~d) +
                          (a*~b*c*d) + (a*b*~c*~d) + (a*b*c*d)''', simplify=True)
        self.assertEqual(expected.pretty(), result.pretty())

    @expectedFailure
    def test_simplify_complex_expression_parsed_with_simplify(self):
        # FIXME: THIS SHOULD NOT FAIL
        a = boolean.Symbol('a')
        b = boolean.Symbol('b')
        c = boolean.Symbol('c')
        d = boolean.Symbol('d')

        test_expression_str = '''
            (~a*~b*~c*~d) + (~a*~b*~c*d) + (~a*b*~c*~d) +
            (~a*b*c*d) + (~a*b*~c*d) + (~a*b*c*~d) +
            (a*~b*~c*d) + (~a*b*c*d) + (a*~b*c*d) + (a*b*c*d)
            '''

        parsed = boolean.Expression().parse(test_expression_str, simplify=True)

        test_expression = (
            (~a * ~b * ~c * ~d) + (~a * ~b * ~c * d) + (~a * b * ~c * ~d) +
            (~a * b * c * d) + (~a * b * ~c * d) + (~a * b * c * ~d) +
            (a * ~b * ~c * d) + (~a * b * c * d) + (a * ~b * c * d) + (a * b * c * d)
        ).simplify()

        # we have a different simplify behavior for expressions built from python expressions
        # vs. expression built from an object tree vs. expression built from a parse
        self.assertEqual(parsed.pretty(), test_expression.pretty())

    @expectedFailure
    def test_complex_expression_without_parens_parsed_or_built_in_python_should_be_identical(self):
        # FIXME: THIS SHOULD NOT FAIL
        a = boolean.Symbol('a')
        b = boolean.Symbol('b')
        c = boolean.Symbol('c')
        d = boolean.Symbol('d')

        test_expression_str = '''
            ~a*~b*~c*~d + ~a*~b*~c*d + ~a*b*~c*~d +
            ~a*b*c*d + ~a*b*~c*d + ~a*b*c*~d +
            a*~b*~c*d + ~a*b*c*d + a*~b*c*d + a*b*c*d
            '''

        parsed = boolean.Expression().parse(test_expression_str, simplify=False)

        test_expression = (
            ~a * ~b * ~c * ~d + ~a * ~b * ~c * d + ~a * b * ~c * ~d +
            ~ a * b * c * d + ~a * b * ~c * d + ~a * b * c * ~d +
            a * ~b * ~c * d + ~a * b * c * d + a * ~b * c * d + a * b * c * d
        )

        self.assertEqual(parsed.pretty(), test_expression.pretty())

    @expectedFailure
    def test_simplify_complex_expression_parsed_then_simplified(self):
        # FIXME: THIS SHOULD NOT FAIL

        a = boolean.Symbol('a')
        b = boolean.Symbol('b')
        c = boolean.Symbol('c')
        d = boolean.Symbol('d')

        parse = boolean.Expression().parse

        test_expression_str = ''.join('''
            (~a*~b*~c*~d) + (~a*~b*~c*d) + (~a*b*~c*~d) +
            (~a*b*c*d) + (~a*b*~c*d) + (~a*b*c*~d) +
            (a*~b*~c*d) + (~a*b*c*d) + (a*~b*c*d) + (a*b*c*d)
        '''.split())

        test_expression = (
            (~a * ~b * ~c * ~d) + (~a * ~b * ~c * d) + (~a * b * ~c * ~d) +
            (~a * b * c * d) + (~a * b * ~c * d) + (~a * b * c * ~d) +
            (a * ~b * ~c * d) + (~a * b * c * d) + (a * ~b * c * d) + (a * b * c * d)
        )

        parsed = parse(test_expression_str, simplify=False)
        self.assertEqual(test_expression_str, str(parsed))

        expected = (a * ~b * d) + (~a * b) + (~a * ~c) + (b * c * d)
        self.assertEqual(expected.pretty(), test_expression.simplify().pretty())

        parsed = parse(test_expression_str, simplify=True)

        # FIXME: THIS SHOULD NOT FAIL
        # we have a different simplify behavior for expressions built from python expressions
        # vs. expression built from an object tree vs. expression built from a parse
        self.assertEqual(expected.simplify().pretty(), parsed.simplify().pretty())

        expected_str = '(a*~b*d)+(~a*b)+(~a*~c)+(b*c*d)'
        self.assertEqual(expected_str, str(parsed))

        parsed2 = parse(test_expression_str, simplify=False)
        self.assertEqual(expected.pretty(), parsed2.simplify().pretty())

        self.assertEqual(expected_str, str(parsed2.simplify()))

        expected = boolean.OR(
            boolean.AND(
                boolean.NOT(boolean.Symbol('a')),
                boolean.NOT(boolean.Symbol('b')),
                boolean.NOT(boolean.Symbol('c')),
                boolean.NOT(boolean.Symbol('d'))
            ),
            boolean.AND(
                boolean.NOT(boolean.Symbol('a')),
                boolean.NOT(boolean.Symbol('b')),
                boolean.NOT(boolean.Symbol('c')),
                boolean.Symbol('d')
            ),
            boolean.AND(
                boolean.NOT(boolean.Symbol('a')),
                boolean.Symbol('b'),
                boolean.NOT(boolean.Symbol('c')),
                boolean.NOT(boolean.Symbol('d'))
            ),
            boolean.AND(
                boolean.NOT(boolean.Symbol('a')),
                boolean.Symbol('b'),
                boolean.Symbol('c'),
                boolean.Symbol('d')),
            boolean.AND(
                boolean.NOT(boolean.Symbol('a')),
                boolean.Symbol('b'),
                boolean.NOT(boolean.Symbol('c')),
                boolean.Symbol('d')
            ),
            boolean.AND(
                boolean.NOT(boolean.Symbol('a')),
                boolean.Symbol('b'),
                boolean.Symbol('c'),
                boolean.NOT(boolean.Symbol('d'))
            ),
            boolean.AND(
                boolean.Symbol('a'),
                boolean.NOT(boolean.Symbol('b')),
                boolean.NOT(boolean.Symbol('c')),
                boolean.Symbol('d')
            ),
            boolean.AND(
                boolean.NOT(boolean.Symbol('a')),
                boolean.Symbol('b'),
                boolean.Symbol('c'),
                boolean.Symbol('d')
            ),
            boolean.AND(
                boolean.Symbol('a'),
                boolean.NOT(boolean.Symbol('b')),
                boolean.Symbol('c'),
                boolean.Symbol('d')
            ),
            boolean.AND(
                boolean.Symbol('a'),
                boolean.Symbol('b'),
                boolean.Symbol('c'),
                boolean.Symbol('d')
            )
        )

        result = boolean.Expression().parse(test_expression_str)
        result = result.simplify()
        self.assertEqual(expected, result)

    def test_subtract(self):
        parse = boolean.Expression().parse
        expr = parse('a*b*c')
        p1 = parse('b*d')
        p2 = parse('a*c')
        result = parse('b')
        self.assertEqual(expr.subtract(p1, simplify=True), expr)
        self.assertEqual(expr.subtract(p2, simplify=True), result)

    def test_flatten(self):
        parse = boolean.Expression().parse

        t1 = parse('a * (b*c)', simplify=False)
        t2 = parse('a*b*c', simplify=False)
        self.assertNotEqual(t1, t2)
        self.assertEqual(t1.flatten(), t2)

        t1 = parse('a + ((b*c) + (a*c)) + b', simplify=False)
        t2 = parse('a + (b*c) + (a*c) + b', simplify=False)
        self.assertNotEqual(t1, t2)
        self.assertEqual(t1.flatten(), t2)

    def test_distributive(self):
        a = self.a
        b = self.b
        c = self.c
        d = boolean.Symbol('d')
        e = boolean.Symbol('e')
        self.assertEqual((a * (b + c)).distributive(), (a * b) + (a * c))
        t1 = boolean.AND(a, (b + c), (d + e))
        t2 = boolean.OR(boolean.AND(a, b, d), boolean.AND(a, b, e), boolean.AND(a, c, d), boolean.AND(a, c, e))
        self.assertEqual(t1.distributive(), t2)

    def test_equal(self):
        from boolean.boolean import DualBase

        t1 = DualBase(self.b, self.a)
        t2 = DualBase(self.b, self.c, self.a)
        # Test __eq__.
        self.assertEqual(t1, t1)
        self.assertEqual(self.t1, t1)
        self.assertEqual(self.t2, t2)
        self.assertNotEqual(t1, t2)
        self.assertNotEqual(t1, 1)
        self.assertFalse(t1 is True)
        self.assertFalse(t1 is None)
        # Test __ne__.
        self.assertFalse(t1 != t1)
        self.assertFalse(self.t1 != t1)
        self.assertFalse(self.t2 != t2)
        self.assertTrue(t1 != t2)
        self.assertTrue(t1 != 1)
        self.assertTrue(t1 is not True)
        self.assertTrue(t1 is not None)

    def test_order(self):
        x, y, z = boolean.Symbol(1), boolean.Symbol(2), boolean.Symbol(3)
        self.assertTrue(boolean.AND(x, y) < boolean.AND(x, y, z))
        self.assertTrue(not boolean.AND(x, y) > boolean.AND(x, y, z))
        self.assertTrue(boolean.AND(x, y) < boolean.AND(x, z))
        self.assertTrue(not boolean.AND(x, y) > boolean.AND(x, z))
        self.assertTrue(boolean.AND(x, y) < boolean.AND(y, z))
        self.assertTrue(not boolean.AND(x, y) > boolean.AND(y, z))
        self.assertTrue(not boolean.AND(x, y) < boolean.AND(x, y))
        self.assertTrue(not boolean.AND(x, y) > boolean.AND(x, y))

    def test_printing(self):
        parse = boolean.Expression().parse
        self.assertEqual(str(parse('a*a', simplify=False)), 'a*a')
        self.assertEqual(repr(parse('a*a', simplify=False)), "AND(Symbol('a'), Symbol('a'))")
        self.assertEqual(str(parse('a+a', simplify=False)), 'a+a')
        self.assertEqual(repr(parse('a+a', simplify=False)), "OR(Symbol('a'), Symbol('a'))")
        self.assertEqual(str(parse('(a+b)*c', simplify=False)), '(a+b)*c')
        self.assertEqual(repr(parse('(a+b)*c', simplify=False)), "AND(OR(Symbol('a'), Symbol('b')), Symbol('c'))")


class OtherTestCase(unittest.TestCase):

    def test_class_order(self):
        # FIXME: this test is cryptic: what does it do?
        parse = boolean.Expression().parse
        order = (
            (boolean.TRUE(), boolean.FALSE()),
            (boolean.Symbol('y'), boolean.Symbol('x')),
            (parse('x*y'),),
            (parse('x+y'),),
        )
        for i, tests in enumerate(order):
            for case1 in tests:
                for j in range(i + 1, len(order)):
                    for case2 in order[j]:

                        self.assertTrue(case1 < case2)
                        self.assertTrue(case2 > case1)

    def test_parse(self):
        a, b, c = boolean.Symbol('a'), boolean.Symbol('b'), boolean.Symbol('c')
        parse = boolean.Expression().parse
        self.assertEqual(parse('0'), boolean.FALSE())
        self.assertEqual(parse('(0)'), boolean.FALSE())
        self.assertEqual(parse('1') , boolean.TRUE())
        self.assertEqual(parse('(1)'), boolean.TRUE())
        self.assertEqual(parse('a'), a)
        self.assertEqual(parse('(a)'), a)
        self.assertEqual(parse('(a)'), a)
        self.assertEqual(parse('~a'), parse('~(a)'))
        self.assertEqual(parse('~(a)'), parse('(~a)'))
        self.assertEqual(parse('~a'), ~a)
        self.assertEqual(parse('(~a)'), ~a)
        self.assertEqual(parse('~~a'), (~~a).simplify())
        self.assertEqual(parse('a*b'), a * b)
        self.assertEqual(parse('~a*b'), ~a * b)
        self.assertEqual(parse('a*~b'), a * ~b)
        self.assertEqual(parse('a*b*c'), parse('a*b*c'))
        self.assertEqual(parse('a*b*c'), boolean.AND(a, b, c))
        self.assertEqual(parse('~a*~b*~c'), parse('~a*~b*~c'))
        self.assertEqual(parse('~a*~b*~c'), boolean.AND(~a, ~b, ~c))
        self.assertEqual(parse('a+b'), a + b)
        self.assertEqual(parse('~a+b'), ~a + b)
        self.assertEqual(parse('a+~b'), a + ~b)
        self.assertEqual(parse('a+b+c'), parse('a+b+c'))
        self.assertEqual(parse('a+b+c'), boolean.OR(a, b, c))
        self.assertEqual(parse('~a+~b+~c'), boolean.OR(~a, ~b, ~c))
        self.assertEqual(parse('(a+b)'), a + b)
        self.assertEqual(parse('a*(a+b)'), (a * (a + b)).simplify())
        self.assertEqual(parse('a*(a+~b)'), (a * (a + ~b)).simplify())
        self.assertEqual(parse('(a*b)+(b*((c+a)*(b+(c*a))))'), ((a * b) + (b * ((c + a) * (b + (c * a))))).simplify())
        self.assertEqual(parse('(a*b)+(b*((c+a)*(b+(c*a))))'), parse('a*b + b*(c+a)*(b+c*a)'))

    def test_subs(self):
        a, b, c = boolean.Symbol('a'), boolean.Symbol('b'), boolean.Symbol('c')
        expr = a * b + c
        self.assertEqual(expr.subs({a: b}).simplify(), b + c)
        self.assertEqual(expr.subs({a: a}).simplify(), expr)
        self.assertEqual(expr.subs({a: b + c}).simplify(), boolean.Expression().parse('(b+c)*b+c').simplify())
        self.assertEqual(expr.subs({a * b: a}).simplify(), a + c)
        self.assertEqual(expr.subs({c: boolean.TRUE()}).simplify(), boolean.TRUE())

    def test_normalize(self):
        base = boolean.Expression()
        expr = base.parse('((s+a)*(s+b)*(s+c)*(s+d)*(e+c+d))+(a*e*d)')
        result = expr.normalize(expr.AND)
        expected = base.parse('(a+s)*(b+e+s)*(c+d+e)*(c+e+s)*(d+s)')
        self.assertEqual(result, expected)


class BooleanBoolTestCase(unittest.TestCase):

    def test_bool(self):
        a, b, c = boolean.Symbol('a'), boolean.Symbol('b'), boolean.Symbol('c')
        expr = a * b + c
        self.assertRaises(TypeError, bool, expr.subs({a: boolean.TRUE()}, simplify=False))
        self.assertRaises(TypeError, bool, expr.subs({b: boolean.TRUE()}, simplify=False))
        self.assertRaises(TypeError, bool, expr.subs({c: boolean.TRUE()}, simplify=False))
        self.assertRaises(TypeError, bool, expr.subs({a: boolean.TRUE(), b: boolean.TRUE()}, simplify=False))
        result = expr.subs({c:boolean.TRUE()}, simplify=True)
        result = result.simplify()
        self.assertTrue(result)

        result = expr.subs({a:boolean.TRUE(), b:boolean.TRUE()}, simplify=True)
        result = result.simplify()
        self.assertTrue(result)


class CustomSymbolTestCase(unittest.TestCase):

    def test_custom_symbol(self):
        class CustomSymbol(boolean.Symbol):
            def __init__(self, name, value='value'):
                self.var = value
                super(CustomSymbol, self).__init__(name)
        try:
            CustomSymbol('a', value='This is A')
        except TypeError as e:
            self.fail(e)


if __name__ == '__main__':
    unittest.main()
