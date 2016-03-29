"""
Boolean Algebra.

Tests

Copyright (c) 2009-2010 Sebastian Kraemer, basti.kr@gmail.com
Released under revised BSD license.
"""

from __future__ import absolute_import, unicode_literals

import unittest
from unittest.case import expectedFailure

import tokenize

try:
    from io import StringIO
except ImportError:
    try:
        from cStringIO import StringIO
    except ImportError:
        from StringIO import StringIO

try:
    basestring  # Python 2
except NameError:
    basestring = str  # Python 3

import boolean


class ExpressionTestCase(unittest.TestCase):

    def test_creation(self):
        E = boolean.Expression
        expr_str = "(a+b+c)*d*(~e+(f*g))"
        expr = boolean.parse(expr_str)
        self.assertTrue(E(expr) is expr)
        self.assertTrue(E(expr_str) == expr)
        self.assertTrue(E(1) is boolean.TRUE)
        self.assertTrue(E(True) is boolean.TRUE)
        self.assertTrue(E(0) is boolean.FALSE)
        self.assertTrue(E(False) is boolean.FALSE)

    def test_parse_with_mixed_operators_multilines_and_custom_symbol(self):

        class MySymbol(boolean.Symbol):
            pass

        expr_str = """(a or ~ b +_c  ) and #some comment
                      d & ( ! e_
                      | (my * g OR 1 or 0) ) AND that """

        expr = boolean.parse(expr_str, simplify=False, symbol_class=MySymbol)

        expected = boolean.AND(
            boolean.OR(
                MySymbol('a'),
                boolean.NOT(boolean.Symbol('b')),
                MySymbol('_c'),
                simplify=False
            ),
            MySymbol('d'),
            boolean.OR(
                boolean.NOT(MySymbol('e_')),
                boolean.OR(
                    boolean.AND(
                        MySymbol('my'),
                        MySymbol('g'),
                        simplify=False
                    ),
                    boolean.TRUE,
                    boolean.FALSE,
                    simplify=False
                ),
                simplify=False
            ),
            MySymbol('that'),
            simplify=False
        )

        self.assertEqual(expected, expr)

    def test_parse_recognizes_trueish_and_falsish_symbol_tokens(self):
        expr_str = 'True or False or None or 0 or 1 or TRue or FalSE or NONe'
        expr = boolean.parse(expr_str, simplify=False)
        expected = boolean.OR(
            boolean.TRUE,
            boolean.FALSE,
            boolean.FALSE,
            boolean.FALSE,
            boolean.TRUE,
            boolean.TRUE,
            boolean.FALSE,
            boolean.FALSE,
            simplify=False
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
                        yield ops[tok], tok, row, col
                    elif tok == 'Custom':
                        yield CustomSymbol(tok), tok, row, col
                    else:
                        yield boolean.Symbol(tok), tok, row, col

        expr_str = """( Custom WHY_NOT regular ) ALSO NEITHER  (
                      not_custom ALSO standard )
                    """

        tokenized = tokenizer(expr_str)
        expr = boolean.parse(tokenized, simplify=False)
        expected = boolean.AND(
            boolean.OR(
                CustomSymbol('Custom'),
                boolean.Symbol('regular'),
                simplify=False
            ),
            boolean.NOT(
                boolean.AND(
                    boolean.Symbol('not_custom'),
                    boolean.Symbol('standard'),
                    simplify=False
                ),
                simplify=False
            ),
            simplify=False
        )
        self.assertEqual(expected, expr)

    def test_parse_with_advanced_tokenizer_example(self):

        class PlainVar(boolean.Symbol):
            "Plain boolean variable"

        class ColonDotVar(boolean.Symbol):
            "Colon and dot-separated string boolean variable"

        def advanced_tokenizer_example(expr):
            """
            Example custom tokenizer derived from the standard tokenizer with
            this extra feature: a colon- and dot-separated string is recognized
            and stored in a custom symbol. Also, in contrast with the standard
            tokenizer, only these boolean operators are recognized : & | ! and
            or not.

            For more advanced tokenization you could also consider forking the
            `tokenize` standard library module.
            """

            if not isinstance(expr, basestring):
                raise TypeError("expr must be string but it is %s." % type(expr))

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
                'true': boolean.TRUE,
                '1': boolean.TRUE,
                'false': boolean.FALSE,
                '0': boolean.FALSE,
                'none': boolean.FALSE,
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
                        yield symbol, dotted, srow, scol
                        dotted, srow, scol = '', None, None

                    continue

                std_token = TOKENS.get(tok.lower())
                if std_token is not None:
                    # we reached a break point and should yield the current dotted
                    symbol = build_symbol(dotted)
                    if symbol is not None:
                        yield symbol, dotted, srow, scol
                        dotted, srow, scol = '', 0, 0

                    yield std_token, tok, row, col

                    continue

                if toktype == tokenize.NAME or (toktype == tokenize.OP and tok in COLON_DOT):
                    if not dotted:
                        srow = row
                        scol = col
                    dotted += tok

                else:
                    raise TypeError('Unknown token: %(tok)r at line: %(row)r, column: %(col)r' % locals())

        test_expr = """
            (colon1:dot1.dot2 or colon2_name:col_on3:do_t1.do_t2.do_t3 )
            and
            ( plain_symbol & !Custom )
        """

        tokenized = advanced_tokenizer_example(test_expr)
        expr = boolean.parse(tokenized, simplify=False)
        expected = boolean.AND(
            boolean.OR(
                ColonDotVar('colon1:dot1.dot2'),
                ColonDotVar('colon2_name:col_on3:do_t1.do_t2.do_t3'),
                simplify=False
            ),
            boolean.AND(
                PlainVar('plain_symbol'),
                boolean.NOT(PlainVar('Custom')),
                simplify=False
            ),
            simplify=False
        )
        self.assertEqual(expected, expr)


class BaseElementTestCase(unittest.TestCase):

    def test_creation(self):
        BE = boolean.BaseElement
        self.assertTrue(BE(1) is boolean.TRUE)
        self.assertTrue(BE(True) is boolean.TRUE)
        self.assertTrue(BE(boolean.TRUE) is boolean.TRUE)
        self.assertTrue(BE(0) is boolean.FALSE)
        self.assertTrue(BE(False) is boolean.FALSE)
        self.assertTrue(BE(boolean.FALSE) is boolean.FALSE)
        self.assertRaises(TypeError, BE)
        self.assertRaises(TypeError, BE, 2)
        self.assertRaises(TypeError, BE, "a")
        self.assertTrue(boolean.TRUE.__class__() is boolean.TRUE)
        self.assertTrue(boolean.FALSE.__class__() is boolean.FALSE)

    def test_literals(self):
        self.assertTrue(boolean.TRUE.literals == set())
        self.assertTrue(boolean.FALSE.literals == set())

    def test_literalize(self):
        self.assertTrue(boolean.TRUE.literalize() is boolean.TRUE)
        self.assertTrue(boolean.FALSE.literalize() is boolean.FALSE)

    def test_simplify(self):
        self.assertTrue(boolean.TRUE.simplify() is boolean.TRUE)
        self.assertTrue(boolean.FALSE.simplify() is boolean.FALSE)

    def test_dual(self):
        self.assertTrue(boolean.TRUE.dual == boolean.FALSE)
        self.assertTrue(boolean.FALSE.dual == boolean.TRUE)

    def test_equality(self):
        self.assertTrue(boolean.TRUE == boolean.TRUE)
        self.assertTrue(boolean.FALSE == boolean.FALSE)
        self.assertFalse(boolean.TRUE == boolean.FALSE)

    def test_order(self):
        self.assertTrue(boolean.FALSE < boolean.TRUE)
        self.assertTrue(boolean.TRUE > boolean.FALSE)

    def test_printing(self):
        self.assertTrue(str(boolean.TRUE) == "1")
        self.assertTrue(str(boolean.FALSE) == "0")
        self.assertTrue(repr(boolean.TRUE) == "TRUE")
        self.assertTrue(repr(boolean.FALSE) == "FALSE")


class SymbolTestCase(unittest.TestCase):

    def test_init(self):
        boolean.Symbol(1)
        boolean.Symbol("a")
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
        self.assertRaises(AttributeError, setattr, l1, "literals", 1)

    def test_literalize(self):
        s = boolean.Symbol(1)
        self.assertTrue(s.literalize() is s)

    def test_simplify(self):
        s = boolean.Symbol(1)
        self.assertTrue(s.simplify() is s)

    def test_equal(self):
        a = boolean.Symbol("a")
        b = boolean.Symbol("a")
        c = boolean.Symbol("b")
        d = boolean.Symbol()
        e = boolean.Symbol()
        # Test __eq__.
        self.assertTrue(a == a)
        self.assertTrue(a == b)
        self.assertFalse(a == c)
        self.assertFalse(b == c)
        self.assertTrue(d == d)
        self.assertFalse(d == e)
        self.assertFalse(a == d)
        # Test __ne__.
        self.assertFalse(a != a)
        self.assertFalse(a != b)
        self.assertTrue(a != c)
        self.assertTrue(b != c)

    def test_order(self):
        S = boolean.Symbol
        self.assertTrue(S("x") < S())
        self.assertTrue(S() > S("x"))
        self.assertTrue(S(1) < S(2))
        self.assertTrue(S(2) > S(1))
        s1, s2 = S(), S()
        self.assertTrue((s1 < s2) == (hash(s1) < hash(s2)))
        self.assertTrue((s1 > s2) == (hash(s1) > hash(s2)))

    def test_printing(self):
        self.assertTrue(str(boolean.Symbol("a")) == "a")
        self.assertTrue(str(boolean.Symbol(1)) == "1")
        self.assertEqual(repr(boolean.Symbol("a")), "Symbol('a')")
        self.assertTrue(repr(boolean.Symbol(1)) == "Symbol(1)")


class NOTTestCase(unittest.TestCase):

    def test_init(self):
        NOT = boolean.NOT
        self.assertRaises(TypeError, NOT)
        self.assertRaises(TypeError, NOT, "a", "b")
        NOT("a")
        self.assertTrue(
            NOT(1) == NOT(True) == NOT(boolean.TRUE) == boolean.FALSE)
        self.assertTrue(
            NOT(0) == NOT(False) == NOT(boolean.FALSE) == boolean.TRUE)

    def test_isliteral(self):
        s = boolean.Symbol(1)
        self.assertTrue(boolean.NOT(s).isliteral)
        self.assertFalse(boolean.parse("~(a+b)").isliteral)

    def test_literals(self):
        a = boolean.Symbol("a")
        l = ~a
        self.assertTrue(l.isliteral)
        self.assertTrue(l in l.literals)
        self.assertTrue(len(l.literals) == 1)
        l = boolean.parse("~(a*a)", simplify=False)
        self.assertFalse(l.isliteral)
        self.assertTrue(a in l.literals)
        self.assertTrue(len(l.literals) == 1)

    def test_literalize(self):
        p = boolean.parse
        self.assertTrue(p("~a").literalize() == p("~a"))
        self.assertTrue(p("~(a*b)").literalize() == p("~a+~b"))
        self.assertTrue(p("~(a+b)").literalize() == p("~a*~b"))

    def test_simplify(self):
        a = boolean.Symbol("a")
        self.assertTrue(~a == ~a)
        self.assertFalse(a == boolean.parse("~~a", simplify=False))
        self.assertTrue(a == ~~a)
        self.assertTrue(~a == ~~~a)
        self.assertTrue(a == ~~~~a)
        self.assertTrue(~(a * a * a) == ~(a * a * a))
        self.assertFalse(a == boolean.parse("~ ~a", simplify=False))

    def test_cancel(self):
        a = boolean.Symbol("a")
        parse = lambda x: boolean.parse(x, simplify=False)
        self.assertTrue(~a == (~a).cancel())
        self.assertTrue(a == parse("~~a").cancel())
        self.assertTrue(~a == parse("~~~a").cancel())
        self.assertTrue(a == parse("~~~~a").cancel())

    def test_demorgan(self):
        a, b = boolean.symbols("a", "b")
        parse = lambda x: boolean.parse(x, simplify=False)
        self.assertTrue(parse("~(a*b)").demorgan() == ~a + ~b)
        self.assertTrue(parse("~(a+b+c)").demorgan()
                        == parse("~a*~b*~c"))
        self.assertTrue(parse("~(~a*b)").demorgan() == a + ~b)

    def test_order(self):
        x = boolean.Symbol(1)
        y = boolean.Symbol(2)
        self.assertTrue(x < ~x)
        self.assertTrue(~x > x)
        self.assertTrue(~x < y)
        self.assertTrue(y > ~x)

    def test_printing(self):
        a = boolean.Symbol("a")
        self.assertTrue(str(~a) == "~a")
        self.assertTrue(repr(~a) == "NOT(Symbol('a'))")
        expr = boolean.parse("~(a*a)", simplify=False)
        self.assertTrue(str(expr) == "~(a*a)")
        self.assertTrue(repr(expr) == "NOT(AND(Symbol('a'), Symbol('a')))")


class DualBaseTestCase(unittest.TestCase):

    def setUp(self):
        self.a, self.b, self.c = boolean.symbols("a", "b", "c")
        self.t1 = boolean.DualBase(self.a, self.b, simplify=False)
        self.t2 = boolean.DualBase(self.a, self.b, self.c, simplify=False)
        self.t3 = boolean.DualBase(self.a, self.a, simplify=False)
        self.t4 = boolean.DualBase("a", "b", "c", simplify=False)

    def test_init(self):
        self.assertRaises(TypeError, boolean.DualBase)
        for term in (self.t1, self.t2, self.t3, self.t4):
            self.assertTrue(isinstance(term, boolean.DualBase))

    def test_isliteral(self):
        self.assertTrue(self.t1.isliteral is False)
        self.assertTrue(self.t2.isliteral is False)

    def test_literals(self):
        for term in (self.t1, self.t2, self.t3, self.t4):
            self.assertTrue(self.a in term.literals)
        for term in (self.t1, self.t2, self.t4):
            self.assertTrue(self.b in term.literals)
        for term in (self.t2, self.t4):
            self.assertTrue(self.c in term.literals)

    def test_literalize(self):
        p = boolean.parse
        self.assertTrue(p("a+~(b+c)").literalize() == p("a+(~b*~c)"))

    def test_annihilator(self):
        p = lambda x: boolean.parse(x, simplify=False)
        self.assertTrue(p("a*a").annihilator is boolean.FALSE)
        self.assertTrue(p("a+a").annihilator is boolean.TRUE)

    def test_identity(self):
        self.assertTrue(boolean.parse("a+b").identity is boolean.FALSE)
        self.assertTrue(boolean.parse("a*b").identity is boolean.TRUE)

    def test_dual(self):
        self.assertTrue(boolean.AND.getdual() is boolean.OR)
        self.assertTrue(boolean.OR.getdual() is boolean.AND)
        self.assertTrue(boolean.parse("a+b").dual is boolean.AND)
        self.assertTrue(boolean.parse("a*b").dual is boolean.OR)

    def test_simplify(self):
        a = self.a
        b = self.b
        c = self.c
        _0 = boolean.FALSE
        _1 = boolean.TRUE
        # Idempotence
        self.assertTrue(a == a * a)
        # Idempotence + Associativity
        self.assertTrue(a + b == a + (a + b))
        # Annihilation
        self.assertTrue(_0 == (a * _0))
        self.assertTrue(_1 == (a + _1))
        # Identity
        self.assertTrue(a == (a * _1))
        self.assertTrue(a == (a + _0))
        # Complementation
        self.assertTrue(_0 == a * ~a)
        self.assertTrue(_1 == a + ~a)
        # Absorption
        self.assertTrue(a == a * (a + b))
        self.assertTrue(a == a + (a * b))
        self.assertTrue(b * a == (b * a) + (b * a * c))
        # Elimination
        self.assertTrue(a == (a * ~b) + (a * b))
        expr = boolean.parse("(~a*b*c) + (a*~b*c) + (a*b*~c) + (a*b*c)")
        result = boolean.parse("(a*b)+(b*c)+(a*c)")
        self.assertTrue(expr == result)
        expr = boolean.parse("(~a*b*~c*~d) + (a*~b*~c*~d) + (a*~b*c*~d) +"
                             "(a*~b*c*d) + (a*b*~c*~d) + (a*b*c*d)")
        result = boolean.parse("(~b*~d*a) + (~c*~d*b) + (a*c*d)")
        self.assertTrue(expr == result)
        expr = boolean.parse("(a*b*c*d) + (b*d)")
        result = boolean.parse("b*d")
        self.assertTrue(expr == result)

    @expectedFailure
    def test_simplify_complex_expression_failing(self):
        a = boolean.Symbol('a')
        b = boolean.Symbol('b')
        c = boolean.Symbol('c')
        d = boolean.Symbol('d')

        test_expression = ''.join(("(~a*~b*~c*~d) + (~a*~b*~c*d) + (~a*b*~c*~d) +"
                           "(~a*b*c*d) + (~a*b*~c*d) + (~a*b*c*~d) +"
                           "(a*~b*~c*d) + (~a*b*c*d) + (a*~b*c*d) + (a*b*c*d)").split())

        expected = eval(test_expression, None, dict(a=a, b=b, c=c, d=d))
        parsed = boolean.parse(test_expression, simplify=True)

        # FIXME: THIS SHOULD NOT FAIL
        # we have a different simplify behavior for expressions built from python expressions
        # vs. expression built from an object tree vs. expression built from a parse
        self.assertEqual(str(parsed), str(expected))

    def test_simplify_complex_expression(self):
        a = boolean.Symbol('a')
        b = boolean.Symbol('b')
        c = boolean.Symbol('c')
        d = boolean.Symbol('d')

        test_expression = ''.join(("(~a*~b*~c*~d) + (~a*~b*~c*d) + (~a*b*~c*~d) +"
                           "(~a*b*c*d) + (~a*b*~c*d) + (~a*b*c*~d) +"
                           "(a*~b*~c*d) + (~a*b*c*d) + (a*~b*c*d) + (a*b*c*d)").split())

        expected = (a * ~b * d) + (~a * b) + (~a * ~c) + (b * c * d)
        self.assertEqual(expected, boolean.parse(test_expression, simplify=True))

        expected = '(~a*~b*~c*~d)+(~a*~b*~c*d)+(~a*b*~c*~d)+(~a*b*c*d)+(~a*b*~c*d)+(~a*b*c*~d)+(a*~b*~c*d)+(~a*b*c*d)+(a*~b*c*d)+(a*b*c*d)'
        self.assertEqual(test_expression, str(boolean.parse(test_expression, simplify=False)))

        expected = '(a*~b*d)+(~a*b)+(~a*~c)+(b*c*d)'
        self.assertEqual(expected, str(boolean.parse(test_expression, simplify=True)))

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

        self.assertEqual(expected, boolean.parse(test_expression, simplify=True))

    def test_remove(self):
        expr = boolean.parse("a*b*c")
        p1 = boolean.parse("b*d")
        p2 = boolean.parse("a*c")
        result = boolean.parse("b")
        self.assertTrue(expr.remove(p1) == expr)
        self.assertTrue(expr.remove(p2) == result)

    def test_flatten(self):
        p = lambda x: boolean.parse(x, simplify=False)

        t1 = p("a * (b*c)")
        t2 = p("a*b*c")
        self.assertFalse(t1 == t2)
        self.assertTrue(t1.flatten() == t2)

        t1 = p("a + ((b*c) + (a*c)) + b")
        t2 = p("a + (b*c) + (a*c) + b")
        self.assertFalse(t1 == t2)
        self.assertTrue(t1.flatten() == t2)

    def test_distributive(self):
        a = self.a
        b = self.b
        c = self.c
        d = boolean.Symbol("d")
        e = boolean.Symbol("e")
        self.assertTrue((a * (b + c)).distributive() == (a * b) + (a * c))
        t1 = boolean.AND(a, (b + c), (d + e))
        t2 = boolean.OR(boolean.AND(a, b, d), boolean.AND(a, b, e),
                        boolean.AND(a, c, d), boolean.AND(a, c, e))
        self.assertTrue(t1.distributive() == t2)

    def test_equal(self):
        t1 = boolean.DualBase(self.b, self.a, simplify=False)
        t2 = boolean.DualBase(self.b, self.c, self.a, simplify=False)
        # Test __eq__.
        self.assertTrue(t1 == t1)
        self.assertTrue(self.t1 == t1)
        self.assertTrue(self.t2 == t2)
        self.assertFalse(t1 == t2)
        self.assertFalse(t1 == 1)
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
        x, y, z = boolean.symbols(1, 2, 3)
        self.assertTrue(boolean.AND(x, y) < boolean.AND(x, y, z))
        self.assertTrue(not boolean.AND(x, y) > boolean.AND(x, y, z))
        self.assertTrue(boolean.AND(x, y) < boolean.AND(x, z))
        self.assertTrue(not boolean.AND(x, y) > boolean.AND(x, z))
        self.assertTrue(boolean.AND(x, y) < boolean.AND(y, z))
        self.assertTrue(not boolean.AND(x, y) > boolean.AND(y, z))
        self.assertTrue(not boolean.AND(x, y) < boolean.AND(x, y))
        self.assertTrue(not boolean.AND(x, y) > boolean.AND(x, y))

    def test_printing(self):
        parse = lambda x: boolean.parse(x, simplify=False)
        self.assertTrue(str(parse("a*a")) == "a*a")
        self.assertTrue(repr(parse("a*a")) == "AND(Symbol('a'), Symbol('a'))")
        self.assertTrue(str(parse("a+a")) == "a+a")
        self.assertTrue(repr(parse("a+a")) == "OR(Symbol('a'), Symbol('a'))")
        self.assertTrue(str(parse("(a+b)*c")) == "(a+b)*c")
        self.assertTrue(repr(parse("(a+b)*c")) ==
                        "AND(OR(Symbol('a'), Symbol('b')), Symbol('c'))")


class OtherTestCase(unittest.TestCase):

    def test_class_order(self):
        order = ((boolean.TRUE, boolean.FALSE),
                 (boolean.Symbol(), boolean.Symbol("x")),
                 (boolean.parse("x*y"),),
                 (boolean.parse("x+y"),),
                 )
        for i, tests in enumerate(order):
            for case1 in tests:
                for j in range(i + 1, len(order)):
                    for case2 in order[j]:
                        self.assertTrue(case1 < case2)
                        self.assertTrue(case2 > case1)

    def test_parse(self):
        a, b, c = boolean.symbols("a", "b", "c")
        parse = boolean.parse
        self.assertTrue(parse("0") == parse("(0)") == boolean.FALSE)
        self.assertTrue(parse("1") == parse("(1)") == boolean.TRUE)
        self.assertTrue(parse("a") == parse("(a)") == a)
        self.assertTrue(parse("~a") == parse("~(a)") == parse("(~a)") == ~a)
        self.assertTrue(parse("~~a") == ~ ~a)
        self.assertTrue(parse("a*b") == a * b)
        self.assertTrue(parse("~a*b") == ~a * b)
        self.assertTrue(parse("a*~b") == a * ~b)
        self.assertTrue(parse("a*b*c") == parse("a*b*c", simplify=False) ==
                        boolean.AND(a, b, c))
        self.assertTrue(parse("~a*~b*~c") == parse("~a*~b*~c", simplify=False) ==
                        boolean.AND(~a, ~b, ~c))
        self.assertTrue(parse("a+b") == a + b)
        self.assertTrue(parse("~a+b") == ~a + b)
        self.assertTrue(parse("a+~b") == a + ~b)
        self.assertTrue(parse("a+b+c") == parse("a+b+c", simplify=False) ==
                        boolean.OR(a, b, c))
        self.assertTrue(parse("~a+~b+~c") == boolean.OR(~a, ~b, ~c))
        self.assertTrue(parse("(a+b)") == a + b)
        self.assertTrue(parse("a*(a+b)") == a * (a + b))
        self.assertTrue(parse("a*(a+~b)") == a * (a + ~b))
        self.assertTrue(parse("(a*b)+(b*((c+a)*(b+(c*a))))") ==
                        parse("a*b + b*(c+a)*(b+c*a)") ==
                        (a * b) + (b * ((c + a) * (b + (c * a)))))

    def test_subs(self):
        a, b, c = boolean.symbols("a", "b", "c")
        expr = a * b + c
        self.assertEqual(expr.subs({a: b}), b + c)
        self.assertEqual(expr.subs({a: a}), expr)
        self.assertEqual(expr.subs({a: b + c}), boolean.parse("(b+c)*b+c"))
        self.assertEqual(expr.subs({a * b: a}), a + c)
        self.assertEqual(expr.subs({c: boolean.TRUE}), boolean.TRUE)

    def test_normalize(self):
        parse = boolean.parse
        expr = parse("((s+a)*(s+b)*(s+c)*(s+d)*(e+c+d))+(a*e*d)")
        result = boolean.AND(*boolean.normalize(boolean.AND, expr))
        sol = parse("(a+s)*(b+e+s)*(c+d+e)*(c+e+s)*(d+s)")
        self.assertTrue(result == sol)


class BooleanAlgebraTestCase(unittest.TestCase):

    def test_implementation(self):
        class Filter(boolean.BooleanAlgebra):

            def __init__(self, bool_expr=None):
                boolean.BooleanAlgebra.__init__(self, bool_expr=bool_expr,
                                                bool_base=Filter)

            def simplify(self):
                subs_dict = {}
                for obj in self.bool_expr.objects:
                    subs_dict[obj.bool_expr] = obj.simplify()
                return self.bool_expr.subs(subs_dict)

        class ConstFilter(Filter):

            def __init__(self, value):
                Filter.__init__(self)
                self.value = boolean.BaseElement(value)

            def simplify(self):
                return self.value

        a = ConstFilter(True)
        b = ConstFilter(False)
        self.assertTrue(isinstance(a + b, Filter))
        self.assertEqual((a + b).simplify(), boolean.TRUE)
        self.assertEqual((a * b).simplify(), boolean.FALSE)


class BooleanBoolTestCase(unittest.TestCase):

    def test_bool(self):
        a, b, c = boolean.symbols("a", "b", "c")
        expr = a * b + c
        self.assertRaises(TypeError, bool, expr.subs({a: boolean.TRUE}, simplify=False))
        self.assertRaises(TypeError, bool, expr.subs({b: boolean.TRUE}, simplify=False))
        self.assertRaises(TypeError, bool, expr.subs({c: boolean.TRUE}, simplify=False))
        self.assertRaises(TypeError, bool, expr.subs({a: boolean.TRUE, b: boolean.TRUE}, simplify=False))
        self.assertTrue(expr.subs({c: boolean.TRUE}, simplify=True))
        self.assertTrue(expr.subs({a: boolean.TRUE, b: boolean.TRUE}, simplify=True))


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


if __name__ == "__main__":
    unittest.main()
