"""
Boolean Algebra.

Tests

Copyright (c) 2009-2017 Sebastian Kraemer, basti.kr@gmail.com and others
Released under revised BSD license.
"""

from __future__ import absolute_import
from __future__ import unicode_literals
from __future__ import print_function

import copy
import pytest

from boolean.boolean import BaseElement
from boolean.boolean import PARSE_UNKNOWN_TOKEN

import unittest
from unittest.case import expectedFailure

from boolean import BooleanAlgebra
from boolean import ParseError
from boolean import Symbol
from boolean import TOKEN_NOT
from boolean import TOKEN_AND
from boolean import TOKEN_OR
from boolean import TOKEN_TRUE
from boolean import TOKEN_FALSE
from boolean import TOKEN_SYMBOL
from boolean import TOKEN_LPAR
from boolean import TOKEN_RPAR
from boolean.boolean import PARSE_INVALID_SYMBOL_SEQUENCE
from boolean.boolean import PARSE_INVALID_EXPRESSION
from boolean.boolean import PARSE_INVALID_NESTING

from tests.mock_custom_algebra import CustomAlgebra

from tests.mock_advanced_algebra import AdvancedAlgebra
from tests.mock_advanced_algebra import PlainVar, ColonDotVar

class BooleanAlgebraTestCase(unittest.TestCase):

    def test_creation(self):
        algebra = BooleanAlgebra()
        expr_str = '(a|b|c)&d&(~e|(f&g))'
        expr = algebra.parse(expr_str)
        self.assertEqual(expr_str, str(expr))

    def test_parse_with_mixed_operators_multilines_and_custom_symbol(self):

        class MySymbol(Symbol):
            pass

        expr_str = '''(a or ~ b +_c  ) and
                      d & ( ! e_
                      | (my * g OR 1 or 0) ) AND that '''

        algebra = BooleanAlgebra(Symbol_class=MySymbol)
        expr = algebra.parse(expr_str)

        expected = algebra.AND(
            algebra.OR(
                algebra.Symbol('a'),
                algebra.NOT(algebra.Symbol('b')),
                algebra.Symbol('_c'),
            ),
            algebra.Symbol('d'),
            algebra.OR(
                algebra.NOT(algebra.Symbol('e_')),
                algebra.OR(
                    algebra.AND(
                        algebra.Symbol('my'),
                        algebra.Symbol('g'),
                    ),
                    algebra.TRUE,
                    algebra.FALSE,
                ),
            ),
            algebra.Symbol('that'),
        )

        self.assertEqual(expected.pretty(), expr.pretty())
        self.assertEqual(expected, expr)

    def test_parse_recognizes_trueish_and_falsish_symbol_tokens(self):
        expr_str = 'True or False or None or 0 or 1 or TRue or FalSE or NONe'
        algebra = BooleanAlgebra()
        expr = algebra.parse(expr_str)
        expected = algebra.OR(
            algebra.TRUE,
            algebra.FALSE,
            algebra.FALSE,
            algebra.FALSE,
            algebra.TRUE,
            algebra.TRUE,
            algebra.FALSE,
            algebra.FALSE,
        )
        self.assertEqual(expected, expr)

    def test_parse_can_use_iterable_from_alternative_tokenizer(self):
        expr_str = '''( Custom WHY_NOT regular ) ALSO NEITHER  (
                      not_custom ALSO standard )
                   '''

        algebra = CustomAlgebra()
        expr = algebra.parse(expr_str)
        expected = algebra.AND(
            algebra.OR(
                algebra.Symbol('Custom'),
                algebra.Symbol('regular'),
            ),
            algebra.NOT(
                algebra.AND(
                    algebra.Symbol('not_custom'),
                    algebra.Symbol('standard'),
                ),
            ),
        )
        self.assertEqual(expected, expr)

    def test_parse_with_advanced_tokenizer_example(self):
        test_expr = '''
            (colon1:dot1.dot2 or colon2_name:col_on3:do_t1.do_t2.do_t3 )
            and
            ( plain_symbol & !Custom )
        '''

        algebra = AdvancedAlgebra()
        expr = algebra.parse(test_expr)
        expected = algebra.AND(
            algebra.OR(
                ColonDotVar('colon1:dot1.dot2'),
                ColonDotVar('colon2_name:col_on3:do_t1.do_t2.do_t3')
            ),
            algebra.AND(
                PlainVar('plain_symbol'),
                algebra.NOT(PlainVar('Custom'))
            )
        )
        self.assertEqual(expected, expr)

    def test_parse_raise_ParseError(self):
        algebra = BooleanAlgebra()
        invalid_expressions = [
            'l-a AND none',
            '(l-a + AND l-b',
            '(l-a + AND l-b)',
            '(l-a AND l-b',
            '(l-a + AND l-b))',
            '(l-a  AND l-b))',
            'l-a AND',
            'OR l-a',
            '+ l-a',
        ]

        for expr in invalid_expressions:
            try:
                algebra.parse(expr)
                self.fail("Exception should be raised when parsing '%s'" % expr)
            except ParseError as pe:
                assert pe.error_code == PARSE_UNKNOWN_TOKEN

    def test_parse_side_by_side_symbols_should_raise_exception_but_not(self):
        algebra = BooleanAlgebra()
        expr_str = 'a or b c'
        try:
            algebra.parse(expr_str)
        except ParseError as pe:
            assert pe.error_code == PARSE_INVALID_SYMBOL_SEQUENCE

    def test_parse_side_by_side_symbols_should_raise_exception_but_not2(self):
        algebra = BooleanAlgebra()
        expr_str = '(a or b) c'
        try:
            algebra.parse(expr_str)
        except ParseError as pe:
            assert pe.error_code == PARSE_INVALID_EXPRESSION

    def test_parse_side_by_side_symbols_raise_exception(self):
        algebra = BooleanAlgebra()
        expr_str = 'a b'
        try:
            algebra.parse(expr_str)
        except ParseError as pe:
            assert pe.error_code == PARSE_INVALID_SYMBOL_SEQUENCE

    def test_parse_side_by_side_symbols_with_parens_raise_exception(self):
        algebra = BooleanAlgebra()
        expr_str = '(a) (b)'
        try:
            algebra.parse(expr_str)
        except ParseError as pe:
            assert pe.error_code == PARSE_INVALID_NESTING

class TestBaseElement:

    def test_base_element_raises(self):
        with pytest.raises(TypeError):
            BaseElement(2)

        with pytest.raises(TypeError):
            BaseElement('a')

    def test_true_and_false(self):
        algebra = BooleanAlgebra()

        assert algebra.TRUE is algebra.TRUE
        assert algebra.FALSE is algebra.FALSE

        assert algebra.TRUE == algebra.TRUE
        assert algebra.FALSE == algebra.FALSE

        assert algebra.TRUE is not algebra.FALSE
        assert algebra.FALSE is not algebra.TRUE

        assert algebra.TRUE != algebra.FALSE
        assert algebra.FALSE != algebra.TRUE

        assert bool(algebra.TRUE) is True
        assert bool(algebra.FALSE) is False

        assert algebra.TRUE == True
        assert algebra.FALSE == False

    def test_literals(self):
        algebra = BooleanAlgebra()

        assert algebra.TRUE.literals == set()
        assert algebra.FALSE.literals == set()

    def test_literalize(self):
        algebra = BooleanAlgebra()

        assert algebra.TRUE.literalize() == algebra.TRUE
        assert algebra.FALSE.literalize() == algebra.FALSE

        assert algebra.TRUE.literalize() != algebra.FALSE
        assert algebra.FALSE.literalize() != algebra.TRUE

    def test_simplify(self):
        algebra = BooleanAlgebra()

        assert algebra.TRUE.simplify() == algebra.TRUE
        assert algebra.FALSE.simplify() == algebra.FALSE

    def test_dual(self):
        algebra = BooleanAlgebra()

        assert algebra.TRUE.dual ==  algebra.FALSE
        assert algebra.FALSE.dual ==  algebra.TRUE

    def test_equality(self):
        algebra = BooleanAlgebra()

        assert algebra.TRUE == algebra.TRUE
        assert algebra.FALSE == algebra.FALSE

        assert      algebra.TRUE != algebra.FALSE
        assert not (algebra.TRUE == algebra.FALSE)

    def test_order(self):
        algebra = BooleanAlgebra()

        assert algebra.FALSE < algebra.TRUE
        assert algebra.TRUE > algebra.FALSE

    def test_printing(self):
        algebra = BooleanAlgebra()

        assert str(algebra.TRUE) == '1'
        assert str(algebra.FALSE) == '0'

        assert repr(algebra.TRUE) == 'TRUE'
        assert repr(algebra.FALSE) == 'FALSE'

class TestSymbolCase:

    def test_isliteral(self):
        assert Symbol(1).isliteral is True

    def test_literals(self):
        l1 = Symbol(1)
        l2 = Symbol(1)

        assert l1 in l1.literals
        assert l1 in l2.literals
        assert l2 in l1.literals
        assert l2 in l2.literals

        for symbol in [l1, l2]:
            with pytest.raises(AttributeError):
                symbol.setattr('literals', 1)

    def test_literalize(self):
        s = Symbol(1)

        assert s.literalize() == s

    def test_simplify(self):
        s = Symbol(1)

        assert s.simplify() == s

    def test_symbols_eq_0(self):
        algebra = BooleanAlgebra()

        a = algebra.Symbol('a')

        assert a == a

    def test_symbols_eq_1(self):
        algebra = BooleanAlgebra()

        a0 = algebra.Symbol('a')
        a1 = algebra.Symbol('a')

        assert a0 == a1
        assert a1 == a0

    def test_symbols_eq_2(self):
        algebra = BooleanAlgebra()

        a = algebra.Symbol('a')
        b = algebra.Symbol('b')

        assert not (a == b)
        assert not (b == a)

    def test_symbols_eq_3(self):
        algebra = BooleanAlgebra()

        assert      algebra.Symbol('a') == algebra.Symbol('a')
        assert not (algebra.Symbol('a') == algebra.Symbol('b'))

    def test_symbols_ne_0(self):
        algebra = BooleanAlgebra()

        a = algebra.Symbol('a')

        assert not (a != a)

    def test_symbols_ne_1(self):
        algebra = BooleanAlgebra()

        a0 = algebra.Symbol('a')
        a1 = algebra.Symbol('a')

        assert not (a0 != a1)
        assert not (a1 != a0)

    def test_symbols_ne_2(self):
        algebra = BooleanAlgebra()

        a = algebra.Symbol('a')
        b = algebra.Symbol('b')

        assert a != b
        assert b != a

    def test_symbols_ne_3(self):
        algebra = BooleanAlgebra()

        assert not (algebra.Symbol('a') != algebra.Symbol('a'))
        assert      algebra.Symbol('a') != algebra.Symbol('b')

    def test_symbols_eq_ne(self):
        algebra = BooleanAlgebra()

        symbols0 = [
            algebra.Symbol('knights'),
            algebra.Symbol('who'),
            algebra.Symbol('say'),
            algebra.Symbol('ni!'),
            algebra.Symbol('Beautiful is better than ugly.'),
            algebra.Symbol('Explicit is better than implicit.'),
            algebra.Symbol('0'),
            algebra.Symbol('1'),
            algebra.Symbol('^'),
            algebra.Symbol(-1),
            algebra.Symbol(0),
            algebra.Symbol(1),
            algebra.Symbol('123'),
            algebra.Symbol('!!!'),
        ]

        symbols1 = copy.deepcopy(symbols0)

        for symbol in symbols0:
            assert      symbol == symbol
            assert not (symbol != symbol)

        for symbol0, symbol1 in zip(symbols0, symbols1):
            assert symbol0 == symbol1
            assert symbol1 == symbol0

            assert not (symbol0 != symbol1)
            assert not (symbol1 != symbol0)

        for i in range(len(symbols0)):
            for j in range(i + 1, len(symbols0)):
                assert not (symbols0[i] == symbols1[j])
                assert not (symbols1[j] == symbols0[i])

                assert symbols0[i] != symbols1[j]
                assert symbols1[j] != symbols0[i]

    def test_order(self):
        assert Symbol(-1) < Symbol(0)
        assert Symbol(0) > Symbol(-1)

        assert Symbol(1) < Symbol(2)
        assert Symbol(2) > Symbol(1)

        assert Symbol('x') < Symbol('y')
        assert Symbol('y') > Symbol('x')

    def test_printing(self):
        assert 'a' == str(Symbol('a'))
        assert "Symbol('a')" == repr(Symbol('a'))

        assert '1' == str(Symbol(1))
        assert 'Symbol(1)' == repr(Symbol(1))

        assert '-1' == str(Symbol(-1))
        assert 'Symbol(-1)' == repr(Symbol(-1))

class TestNOT:

    def test_raises(self):
        algebra = BooleanAlgebra()

        with pytest.raises(TypeError):
            algebra.NOT()

        with pytest.raises(TypeError):
            algebra.NOT('a', 'b')

    def test_true_and_false(self):
        algebra = BooleanAlgebra()

        assert algebra.TRUE is (algebra.NOT(algebra.FALSE)).simplify()
        assert algebra.FALSE is (algebra.NOT(algebra.TRUE)).simplify()

        assert algebra.TRUE == (algebra.NOT(algebra.FALSE)).simplify()
        assert algebra.FALSE == (algebra.NOT(algebra.TRUE)).simplify()

    def test_isliteral(self):
        algebra = BooleanAlgebra()

        s = algebra.Symbol(1)

        # negation of a literal is still a literal
        assert algebra.NOT(s).isliteral
        # negation of a non-literal is still a non-literal
        assert not algebra.parse('~(a|b)').isliteral

    def test_literals_0(self):
        algebra = BooleanAlgebra()

        a = algebra.Symbol('a')
        b = ~a

        assert a.isliteral
        assert b.isliteral

        assert a in a.literals
        assert b in b.literals

        assert len(a.literals) == 1
        assert len(b.literals) == 1

    def test_literals_1(self):
        algebra = BooleanAlgebra()

        expression = algebra.parse('~(a&a)')

        assert not expression.isliteral

        assert algebra.Symbol('a') in expression.literals
        assert len(expression.literals) == 1

    def test_literals_2(self):
        algebra = BooleanAlgebra()

        expression = algebra.parse('~(a&a)', simplify=True)

        assert expression.isliteral
        assert expression == algebra.NOT(algebra.Symbol('a'))

    def test_literalize(self):
        parse = BooleanAlgebra().parse

        assert parse('~a').literalize() == parse('~a')
        assert parse('~(a&b)').literalize() ==  parse('~a|~b')
        assert parse('~(a|b)').literalize() ==  parse('~a&~b')

    def test_invert_eq_not(self):
        algebra = BooleanAlgebra()

        a = algebra.Symbol('a')

        assert ~a == ~a
        assert ~a == algebra.NOT(a)

    def test_simplify(self):
        algebra = BooleanAlgebra()

        a = algebra.Symbol('a')

        assert a == a.simplify()
        assert a == (~~a).simplify()
        assert a == (~~ ~~a).simplify()

        assert ~a == (~a).simplify()
        assert ~a == (~ ~~a).simplify()
        assert ~a == (~ ~~ ~~a).simplify()

        assert (~(a & a & a)).simplify() == (~(a & a & a)).simplify()
        assert (~(a | a | a)).simplify() == (~(a | a | a)).simplify()

    def test_cancel_0(self):
        """
        Test .cancel() on python variables
        """
        algebra = BooleanAlgebra()

        a = algebra.Symbol('a')

        assert a == (~~a).cancel()
        assert a == (~~ ~~a).cancel()

        assert ~a == (~a).cancel()
        assert ~a == (~ ~~a).cancel()
        assert ~a == (~ ~~ ~~a).cancel()

    def test_cancel_1(self):
        """
        Test .cancel() on .parse() results
        """
        parse = BooleanAlgebra().parse

        assert parse('a') == parse('~~a').cancel()
        assert parse('a') == parse('~~ ~~a').cancel()

        assert parse('~a') == parse('~a').cancel()
        assert parse('~a') == parse('~ ~~a').cancel()
        assert parse('~a') == parse('~ ~~ ~~a').cancel()

    def test_cancel_2(self):
        """
        Test .cancel() on both Python variables and .parse() results
        """
        algebra = BooleanAlgebra()

        a, parse = algebra.Symbol('a'), algebra.parse

        assert a == parse('~~a').cancel()
        assert a == parse('~~ ~~a').cancel()

        assert ~a == parse('~a').cancel()
        assert ~a == parse('~ ~~a').cancel()
        assert ~a == parse('~ ~~ ~~a').cancel()

    def test_demorgan(self):
        parse = BooleanAlgebra().parse

        assert parse('~(a & a)').demorgan() == parse('~a | ~a')

        assert parse('~(a & b)').demorgan() == parse('~a | ~b')
        assert parse('~(a & b & c)').demorgan() == parse('~a | ~b | ~c')

        assert parse('~(~a & b)').demorgan() == parse('a | ~b')
        assert parse('~(a & ~b)').demorgan() == parse('~a | b')

    # TODO: enforced order is obscure, must explain
    def test_order(self):
        algebra = BooleanAlgebra()

        x = algebra.Symbol(1)
        y = algebra.Symbol(2)

        assert x < y
        assert y > x

        assert x < ~x
        assert ~x > x

        assert ~x < y
        assert y > ~x

        assert ~y > x
        assert x < ~y

    def test_printing(self):
        algebra = BooleanAlgebra()

        a = algebra.Symbol('a')

        assert str(~a) == '~a'
        assert repr(~a) == "NOT(Symbol('a'))"

        expression = algebra.parse('~(a&a)')

        assert str(expression) == '~(a&a)'
        assert repr(expression) == "NOT(AND(Symbol('a'), Symbol('a')))"

class DualBaseTestCase(unittest.TestCase):

    def test_init(self):
        from boolean.boolean import DualBase
        a, b, c = Symbol('a'), Symbol('b'), Symbol('c')
        t1 = DualBase(a, b)
        t2 = DualBase(a, b, c)
        t3 = DualBase(a, a)
        t4 = DualBase(a, b, c)

        self.assertRaises(TypeError, DualBase)
        for term in (t1, t2, t3, t4):
            self.assertTrue(isinstance(term, DualBase))

    def test_isliteral(self):
        from boolean.boolean import DualBase
        a, b, c = Symbol('a'), Symbol('b'), Symbol('c')
        t1 = DualBase(a, b)
        t2 = DualBase(a, b, c)

        self.assertFalse(t1.isliteral)
        self.assertFalse(t2.isliteral)

    def test_literals(self):
        from boolean.boolean import DualBase
        a, b, c = Symbol('a'), Symbol('b'), Symbol('c')
        t1 = DualBase(a, b)
        t2 = DualBase(a, b, c)
        t3 = DualBase(a, a)
        t4 = DualBase(a, b, c)

        for term in (t1, t2, t3, t4):
            self.assertTrue(a in term.literals)
        for term in (t1, t2, t4):
            self.assertTrue(b in term.literals)
        for term in (t2, t4):
            self.assertTrue(c in term.literals)

    def test_literalize(self):
        parse = BooleanAlgebra().parse
        self.assertEqual(parse('a|~(b|c)').literalize(), parse('a|(~b&~c)'))

    def test_annihilator(self):
        algebra = BooleanAlgebra()
        self.assertEqual(algebra.parse('a&a').annihilator, algebra.FALSE)
        self.assertEqual(algebra.parse('a|a').annihilator, algebra.TRUE)

    def test_identity(self):
        algebra = BooleanAlgebra()
        self.assertEqual(algebra.parse('a|b').identity, algebra.FALSE)
        self.assertEqual(algebra.parse('a&b').identity, algebra.TRUE)

    def test_dual(self):
        algebra = BooleanAlgebra()
        self.assertEqual(algebra.AND(algebra.Symbol('a'), algebra.Symbol('b')).dual, algebra.OR)
        self.assertEqual(algebra.OR(algebra.Symbol('a'), algebra.Symbol('b')).dual, algebra.AND)

        self.assertEqual(algebra.parse('a|b').dual, algebra.AND)
        self.assertEqual(algebra.parse('a&b').dual, algebra.OR)

    def test_simplify(self):
        algebra = BooleanAlgebra()
        a = algebra.Symbol('a')
        b = algebra.Symbol('b')
        c = algebra.Symbol('c')

        _0 = algebra.FALSE
        _1 = algebra.TRUE
        # Idempotence
        self.assertEqual(a, (a & a).simplify())
        # Idempotence + Associativity
        self.assertEqual(a | b, (a | (a | b)).simplify())
        # Annihilation
        self.assertEqual(_0, (a & _0).simplify())
        self.assertEqual(_1, (a | _1).simplify())
        # Identity
        self.assertEqual(a, (a & _1).simplify())
        self.assertEqual(a, (a | _0).simplify())
        # Complementation
        self.assertEqual(_0, (a & ~a).simplify())
        self.assertEqual(_1, (a | ~a).simplify())
        # Absorption
        self.assertEqual(a, (a & (a | b)).simplify())
        self.assertEqual(a, (a | (a & b)).simplify())
        self.assertEqual(b & a, ((b & a) | (b & a & c)).simplify())

        # Elimination
        self.assertEqual(a, ((a & ~b) | (a & b)).simplify())

        expected = algebra.parse('(a&b)|(b&c)|(a&c)')
        result = algebra.parse('(~a&b&c) | (a&~b&c) | (a&b&~c) | (a&b&c)', simplify=True)
        self.assertEqual(expected, result)

        expected = algebra.parse('b&d')
        result = algebra.parse('(a&b&c&d) | (b&d)', simplify=True)
        self.assertEqual(expected, result)

        expected = algebra.parse('(~b&~d&a) | (~c&~d&b) | (a&c&d)', simplify=True)
        result = algebra.parse('''(~a&b&~c&~d) | (a&~b&~c&~d) | (a&~b&c&~d) |
                          (a&~b&c&d) | (a&b&~c&~d) | (a&b&c&d)''', simplify=True)
        self.assertEqual(expected.pretty(), result.pretty())

    @expectedFailure
    def test_parse_complex_expression_should_create_same_expression_as_python(self):
        algebra = BooleanAlgebra()
        a, b, c = algebra.symbols(*'abc')

        test_expression_str = '''(~a | ~b | ~c)'''
        parsed = algebra.parse(test_expression_str)
        test_expression = (~a | ~b | ~c)  # & ~d
        # print()
        # print('parsed')
        # print(parsed.pretty())
        # print('python')
        # print(test_expression.pretty())
        # we have a different behavior for expressions built from python expressions
        # vs. expression built from an object tree vs. expression built from a parse
        self.assertEqual(parsed.pretty(), test_expression.pretty())
        self.assertEqual(parsed, test_expression)

    @expectedFailure
    def test_simplify_complex_expression_parsed_with_simplify(self):
        # FIXME: THIS SHOULD NOT FAIL
        algebra = BooleanAlgebra()
        a = algebra.Symbol('a')
        b = algebra.Symbol('b')
        c = algebra.Symbol('c')
        d = algebra.Symbol('d')

        test_expression_str = '''
            (~a&~b&~c&~d) | (~a&~b&~c&d) | (~a&b&~c&~d) |
            (~a&b&c&d) | (~a&b&~c&d) | (~a&b&c&~d) |
            (a&~b&~c&d) | (~a&b&c&d) | (a&~b&c&d) | (a&b&c&d)
            '''

        parsed = algebra.parse(test_expression_str, simplify=True)

        test_expression = (
            (~a & ~b & ~c & ~d) | (~a & ~b & ~c & d) | (~a & b & ~c & ~d) |
            (~a & b & c & d) | (~a & b & ~c & d) | (~a & b & c & ~d) |
            (a & ~b & ~c & d) | (~a & b & c & d) | (a & ~b & c & d) | (a & b & c & d)
        ).simplify()

        # we have a different simplify behavior for expressions built from python expressions
        # vs. expression built from an object tree vs. expression built from a parse
        self.assertEqual(parsed.pretty(), test_expression.pretty())

    @expectedFailure
    def test_complex_expression_without_parens_parsed_or_built_in_python_should_be_identical(self):
        # FIXME: THIS SHOULD NOT FAIL
        algebra = BooleanAlgebra()
        a = algebra.Symbol('a')
        b = algebra.Symbol('b')
        c = algebra.Symbol('c')
        d = algebra.Symbol('d')

        test_expression_str = '''
            ~a&~b&~c&~d | ~a&~b&~c&d | ~a&b&~c&~d |
            ~a&b&c&d | ~a&b&~c&d | ~a&b&c&~d |
            a&~b&~c&d | ~a&b&c&d | a&~b&c&d | a&b&c&d
            '''

        parsed = algebra.parse(test_expression_str)

        test_expression = (
            ~a & ~b & ~c & ~d | ~a & ~b & ~c & d | ~a & b & ~c & ~d |
            ~ a & b & c & d | ~a & b & ~c & d | ~a & b & c & ~d |
            a & ~b & ~c & d | ~a & b & c & d | a & ~b & c & d | a & b & c & d
        )

        self.assertEqual(parsed.pretty(), test_expression.pretty())

    @expectedFailure
    def test_simplify_complex_expression_parsed_then_simplified(self):
        # FIXME: THIS SHOULD NOT FAIL

        algebra = BooleanAlgebra()
        a = algebra.Symbol('a')
        b = algebra.Symbol('b')
        c = algebra.Symbol('c')
        d = algebra.Symbol('d')
        parse = algebra.parse

        test_expression_str = ''.join('''
            (~a&~b&~c&~d) | (~a&~b&~c&d) | (~a&b&~c&~d) |
            (~a&b&c&d) | (~a&b&~c&d) | (~a&b&c&~d) |
            (a&~b&~c&d) | (~a&b&c&d) | (a&~b&c&d) | (a&b&c&d)
        '''.split())

        test_expression = (
            (~a & ~b & ~c & ~d) | (~a & ~b & ~c & d) | (~a & b & ~c & ~d) |
            (~a & b & c & d) | (~a & b & ~c & d) | (~a & b & c & ~d) |
            (a & ~b & ~c & d) | (~a & b & c & d) | (a & ~b & c & d) | (a & b & c & d)
        )

        parsed = parse(test_expression_str)
        self.assertEqual(test_expression_str, str(parsed))

        expected = (a & ~b & d) | (~a & b) | (~a & ~c) | (b & c & d)
        self.assertEqual(expected.pretty(), test_expression.simplify().pretty())

        parsed = parse(test_expression_str, simplify=True)

        # FIXME: THIS SHOULD NOT FAIL
        # we have a different simplify behavior for expressions built from python expressions
        # vs. expression built from an object tree vs. expression built from a parse
        self.assertEqual(expected.simplify().pretty(), parsed.simplify().pretty())

        expected_str = '(a&~b&d)|(~a&b)|(~a&~c)|(b&c&d)'
        self.assertEqual(expected_str, str(parsed))

        parsed2 = parse(test_expression_str)
        self.assertEqual(expected.pretty(), parsed2.simplify().pretty())

        self.assertEqual(expected_str, str(parsed2.simplify()))

        expected = algebra.OR(
            algebra.AND(
                algebra.NOT(algebra.Symbol('a')),
                algebra.NOT(algebra.Symbol('b')),
                algebra.NOT(algebra.Symbol('c')),
                algebra.NOT(algebra.Symbol('d'))
            ),
            algebra.AND(
                algebra.NOT(algebra.Symbol('a')),
                algebra.NOT(algebra.Symbol('b')),
                algebra.NOT(algebra.Symbol('c')),
                algebra.Symbol('d')
            ),
            algebra.AND(
                algebra.NOT(algebra.Symbol('a')),
                algebra.Symbol('b'),
                algebra.NOT(algebra.Symbol('c')),
                algebra.NOT(algebra.Symbol('d'))
            ),
            algebra.AND(
                algebra.NOT(algebra.Symbol('a')),
                algebra.Symbol('b'),
                algebra.Symbol('c'),
                algebra.Symbol('d')),
            algebra.AND(
                algebra.NOT(algebra.Symbol('a')),
                algebra.Symbol('b'),
                algebra.NOT(algebra.Symbol('c')),
                algebra.Symbol('d')
            ),
            algebra.AND(
                algebra.NOT(algebra.Symbol('a')),
                algebra.Symbol('b'),
                algebra.Symbol('c'),
                algebra.NOT(algebra.Symbol('d'))
            ),
            algebra.AND(
                algebra.Symbol('a'),
                algebra.NOT(algebra.Symbol('b')),
                algebra.NOT(algebra.Symbol('c')),
                algebra.Symbol('d')
            ),
            algebra.AND(
                algebra.NOT(algebra.Symbol('a')),
                algebra.Symbol('b'),
                algebra.Symbol('c'),
                algebra.Symbol('d')
            ),
            algebra.AND(
                algebra.Symbol('a'),
                algebra.NOT(algebra.Symbol('b')),
                algebra.Symbol('c'),
                algebra.Symbol('d')
            ),
            algebra.AND(
                algebra.Symbol('a'),
                algebra.Symbol('b'),
                algebra.Symbol('c'),
                algebra.Symbol('d')
            )
        )

        result = parse(test_expression_str)
        result = result.simplify()
        self.assertEqual(expected, result)

    def test_parse_invalid_nested_and_should_raise_a_proper_exception(self):
        algebra = BooleanAlgebra()
        expr = '''a (and b)'''

        with self.assertRaises(ParseError) as context:
            algebra.parse(expr)

            self.assertEqual(
                context.exception.error_code, PARSE_INVALID_NESTING
            )

    def test_subtract(self):
        parse = BooleanAlgebra().parse
        expr = parse('a&b&c')
        p1 = parse('b&d')
        p2 = parse('a&c')
        result = parse('b')
        self.assertEqual(expr.subtract(p1, simplify=True), expr)
        self.assertEqual(expr.subtract(p2, simplify=True), result)

    def test_flatten(self):
        parse = BooleanAlgebra().parse

        t1 = parse('a & (b&c)')
        t2 = parse('a&b&c')
        self.assertNotEqual(t1, t2)
        self.assertEqual(t1.flatten(), t2)

        t1 = parse('a | ((b&c) | (a&c)) | b')
        t2 = parse('a | (b&c) | (a&c) | b')
        self.assertNotEqual(t1, t2)
        self.assertEqual(t1.flatten(), t2)

    def test_distributive(self):
        algebra = BooleanAlgebra()
        a = algebra.Symbol('a')
        b = algebra.Symbol('b')
        c = algebra.Symbol('c')
        d = algebra.Symbol('d')
        e = algebra.Symbol('e')
        self.assertEqual((a & (b | c)).distributive(), (a & b) | (a & c))
        t1 = algebra.AND(a, (b | c), (d | e))
        t2 = algebra.OR(algebra.AND(a, b, d), algebra.AND(a, b, e), algebra.AND(a, c, d), algebra.AND(a, c, e))
        self.assertEqual(t1.distributive(), t2)

    def test_equal(self):
        from boolean.boolean import DualBase
        a, b, c = Symbol('a'), Symbol('b'), Symbol('c')
        t1 = DualBase(a, b)
        t1_2 = DualBase(b, a)

        t2 = DualBase(a, b, c)
        t2_2 = DualBase(b, c, a)

        # Test __eq__.
        self.assertTrue(t1 == t1)
        self.assertTrue(t1_2 == t1)
        self.assertTrue(t2_2 == t2)
        self.assertFalse(t1 == t2)
        self.assertFalse(t1 == 1)
        self.assertFalse(t1 is True)
        self.assertFalse(t1 is None)

        # Test __ne__.
        self.assertFalse(t1 != t1)
        self.assertFalse(t1_2 != t1)
        self.assertFalse(t2_2 != t2)
        self.assertTrue(t1 != t2)
        self.assertTrue(t1 != 1)
        self.assertTrue(t1 is not True)
        self.assertTrue(t1 is not None)

    def test_order(self):
        algebra = BooleanAlgebra()
        x, y, z = algebra.Symbol(1), algebra.Symbol(2), algebra.Symbol(3)
        self.assertTrue(algebra.AND(x, y) < algebra.AND(x, y, z))
        self.assertTrue(not algebra.AND(x, y) > algebra.AND(x, y, z))
        self.assertTrue(algebra.AND(x, y) < algebra.AND(x, z))
        self.assertTrue(not algebra.AND(x, y) > algebra.AND(x, z))
        self.assertTrue(algebra.AND(x, y) < algebra.AND(y, z))
        self.assertTrue(not algebra.AND(x, y) > algebra.AND(y, z))
        self.assertTrue(not algebra.AND(x, y) < algebra.AND(x, y))
        self.assertTrue(not algebra.AND(x, y) > algebra.AND(x, y))

    def test_printing(self):
        parse = BooleanAlgebra().parse
        self.assertEqual(str(parse('a&a')), 'a&a')
        self.assertEqual(repr(parse('a&a')), "AND(Symbol('a'), Symbol('a'))")
        self.assertEqual(str(parse('a|a')), 'a|a')
        self.assertEqual(repr(parse('a|a')), "OR(Symbol('a'), Symbol('a'))")
        self.assertEqual(str(parse('(a|b)&c')), '(a|b)&c')
        self.assertEqual(repr(parse('(a|b)&c')), "AND(OR(Symbol('a'), Symbol('b')), Symbol('c'))")


class OtherTestCase(unittest.TestCase):

    def test_class_order(self):
        # FIXME: this test is cryptic: what does it do?
        algebra = BooleanAlgebra()
        order = (
            (algebra.TRUE, algebra.FALSE),
            (algebra.Symbol('y'), algebra.Symbol('x')),
            (algebra.parse('x&y'),),
            (algebra.parse('x|y'),),
        )
        for i, tests in enumerate(order):
            for case1 in tests:
                for j in range(i + 1, len(order)):
                    for case2 in order[j]:

                        self.assertTrue(case1 < case2)
                        self.assertTrue(case2 > case1)

    def test_parse(self):
        algebra = BooleanAlgebra()
        a, b, c = algebra.Symbol('a'), algebra.Symbol('b'), algebra.Symbol('c')
        self.assertEqual(algebra.parse('0'), algebra.FALSE)
        self.assertEqual(algebra.parse('(0)'), algebra.FALSE)
        self.assertEqual(algebra.parse('1') , algebra.TRUE)
        self.assertEqual(algebra.parse('(1)'), algebra.TRUE)
        self.assertEqual(algebra.parse('a'), a)
        self.assertEqual(algebra.parse('(a)'), a)
        self.assertEqual(algebra.parse('(a)'), a)
        self.assertEqual(algebra.parse('~a'), algebra.parse('~(a)'))
        self.assertEqual(algebra.parse('~(a)'), algebra.parse('(~a)'))
        self.assertEqual(algebra.parse('~a'), ~a)
        self.assertEqual(algebra.parse('(~a)'), ~a)
        self.assertEqual(algebra.parse('~~a', simplify=True), (~~a).simplify())
        self.assertEqual(algebra.parse('a&b'), a & b)
        self.assertEqual(algebra.parse('~a&b'), ~a & b)
        self.assertEqual(algebra.parse('a&~b'), a & ~b)
        self.assertEqual(algebra.parse('a&b&c'), algebra.parse('a&b&c'))
        self.assertEqual(algebra.parse('a&b&c'), algebra.AND(a, b, c))
        self.assertEqual(algebra.parse('~a&~b&~c'), algebra.parse('~a&~b&~c'))
        self.assertEqual(algebra.parse('~a&~b&~c'), algebra.AND(~a, ~b, ~c))
        self.assertEqual(algebra.parse('a|b'), a | b)
        self.assertEqual(algebra.parse('~a|b'), ~a | b)
        self.assertEqual(algebra.parse('a|~b'), a | ~b)
        self.assertEqual(algebra.parse('a|b|c'), algebra.parse('a|b|c'))
        self.assertEqual(algebra.parse('a|b|c'), algebra.OR(a, b, c))
        self.assertEqual(algebra.parse('~a|~b|~c'), algebra.OR(~a, ~b, ~c))
        self.assertEqual(algebra.parse('(a|b)'), a | b)
        self.assertEqual(algebra.parse('a&(a|b)', simplify=True), (a & (a | b)).simplify())
        self.assertEqual(algebra.parse('a&(a|~b)', simplify=True), (a & (a | ~b)).simplify())
        self.assertEqual(algebra.parse('(a&b)|(b&((c|a)&(b|(c&a))))', simplify=True), ((a & b) | (b & ((c | a) & (b | (c & a))))).simplify())
        self.assertEqual(algebra.parse('(a&b)|(b&((c|a)&(b|(c&a))))', simplify=True), algebra.parse('a&b | b&(c|a)&(b|c&a)', simplify=True))

    def test_subs(self):
        algebra = BooleanAlgebra()
        a, b, c = algebra.Symbol('a'), algebra.Symbol('b'), algebra.Symbol('c')
        expr = a & b | c
        self.assertEqual(expr.subs({a: b}).simplify(), b | c)
        self.assertEqual(expr.subs({a: a}).simplify(), expr)
        self.assertEqual(expr.subs({a: b | c}).simplify(), algebra.parse('(b|c)&b|c').simplify())
        self.assertEqual(expr.subs({a & b: a}).simplify(), a | c)
        self.assertEqual(expr.subs({c: algebra.TRUE}).simplify(), algebra.TRUE)

    def test_subs_default(self):
        algebra = BooleanAlgebra()
        a, b, c = algebra.Symbol('a'), algebra.Symbol('b'), algebra.Symbol('c')
        expr = a & b | c
        self.assertEqual(expr.subs({}, default=algebra.TRUE).simplify(), algebra.TRUE)
        self.assertEqual(expr.subs({a: algebra.FALSE, c: algebra.FALSE}, default=algebra.TRUE).simplify(), algebra.FALSE)
        self.assertEqual(algebra.TRUE.subs({}, default=algebra.FALSE).simplify(), algebra.TRUE)
        self.assertEqual(algebra.FALSE.subs({}, default=algebra.TRUE).simplify(), algebra.FALSE)

    def test_normalize(self):
        algebra = BooleanAlgebra()

        expr = algebra.parse("a&b")
        self.assertEqual(algebra.dnf(expr), expr)
        self.assertEqual(algebra.cnf(expr), expr)

        expr = algebra.parse("a|b")
        self.assertEqual(algebra.dnf(expr), expr)
        self.assertEqual(algebra.cnf(expr), expr)

        expr = algebra.parse("(a&b)|(c&b)")
        result_dnf = algebra.parse("(a&b)|(b&c)")
        result_cnf = algebra.parse("b&(a|c)")
        self.assertEqual(algebra.dnf(expr), result_dnf)
        self.assertEqual(algebra.cnf(expr), result_cnf)

        expr = algebra.parse("(a|b)&(c|b)")
        result_dnf = algebra.parse("b|(a&c)")
        result_cnf = algebra.parse("(a|b)&(b|c)")
        self.assertEqual(algebra.dnf(expr), result_dnf)
        self.assertEqual(algebra.cnf(expr), result_cnf)

        expr = algebra.parse('((s|a)&(s|b)&(s|c)&(s|d)&(e|c|d))|(a&e&d)')
        result = algebra.normalize(expr, expr.AND)
        expected = algebra.parse('(a|s)&(b|e|s)&(c|d|e)&(c|e|s)&(d|s)')
        self.assertEqual(result, expected)

    def test_get_literals_return_all_literals_in_original_order(self):
        alg = BooleanAlgebra()
        exp = alg.parse('a and b or a and c')
        assert [alg.Symbol('a'), alg.Symbol('b'), alg.Symbol('a'), alg.Symbol('c')] == exp.get_literals()

    def test_get_symbols_return_all_symbols_in_original_order(self):
        alg = BooleanAlgebra()
        exp = alg.parse('a and b or True and a and c')
        assert [alg.Symbol('a'), alg.Symbol('b'), alg.Symbol('a'), alg.Symbol('c')] == exp.get_symbols()

    def test_literals_return_set_of_unique_literals(self):
        alg = BooleanAlgebra()
        exp = alg.parse('a and b or a and c')
        assert set([alg.Symbol('a'), alg.Symbol('b'), alg.Symbol('c')]) == exp.literals

    def test_literals_and_negation(self):
        alg = BooleanAlgebra()
        exp = alg.parse('a and not b and not not c')
        assert set([alg.Symbol('a'), alg.parse('not b'), alg.parse('not c')]) == exp.literals

    def test_symbols_and_negation(self):
        alg = BooleanAlgebra()
        exp = alg.parse('a and not b and not not c')
        assert set([alg.Symbol('a'), alg.Symbol('b'), alg.Symbol('c')]) == exp.symbols

    def test_objects_return_set_of_unique_Symbol_objs(self):
        alg = BooleanAlgebra()
        exp = alg.parse('a and b or a and c')
        assert set(['a', 'b', 'c']) == exp.objects


class BooleanBoolTestCase(unittest.TestCase):

    def test_bool(self):
        algebra = BooleanAlgebra()
        a, b, c = algebra.Symbol('a'), algebra.Symbol('b'), algebra.Symbol('c')
        expr = a & b | c
        self.assertRaises(TypeError, bool, expr.subs({a: algebra.TRUE}))
        self.assertRaises(TypeError, bool, expr.subs({b: algebra.TRUE}))
        self.assertRaises(TypeError, bool, expr.subs({c: algebra.TRUE}))
        self.assertRaises(TypeError, bool, expr.subs({a: algebra.TRUE, b: algebra.TRUE}))
        result = expr.subs({c: algebra.TRUE}, simplify=True)
        result = result.simplify()
        self.assertEqual(algebra.TRUE, result)

        result = expr.subs({a: algebra.TRUE, b: algebra.TRUE}, simplify=True)
        result = result.simplify()
        self.assertEqual(algebra.TRUE, result)


class CustomSymbolTestCase(unittest.TestCase):

    def test_custom_symbol(self):
        class CustomSymbol(Symbol):
            def __init__(self, name, value='value'):
                self.var = value
                super(CustomSymbol, self).__init__(name)
        try:
            CustomSymbol('a', value='This is A')
        except TypeError as e:
            self.fail(e)


if __name__ == '__main__':
    unittest.main()
