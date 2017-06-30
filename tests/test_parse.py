import pytest

from boolean import BooleanAlgebra
from boolean import ParseError

class TestParse:
    def test_constant(self):
        algebra = BooleanAlgebra()

        for x in ['0', 'false']:
            assert algebra.parse(x) == algebra.FALSE
            assert algebra.parse('(' + x + ')') == algebra.FALSE
            assert algebra.parse('((' + x + '))') == algebra.FALSE
            assert algebra.parse('(((' + x + ')))') == algebra.FALSE

        for x in ['1', 'true']:
            assert algebra.parse(x) == algebra.TRUE
            assert algebra.parse('(' + x + ')') == algebra.TRUE
            assert algebra.parse('((' + x + '))') == algebra.TRUE
            assert algebra.parse('(((' + x + ')))') == algebra.TRUE

    @pytest.mark.xfail(reason='IndexError indicates parsing problem')
    def test_wrong_braces_0(self):
        parse = BooleanAlgebra().parse

        for expression in ['(', '((', '(((']:
            with pytest.raises(ParseError):
                parse(expression)

        for expression in [')', '))', ')))']:
            with pytest.raises(ParseError):
                parse(expression)

        for expression in ['()', '(())', '((()))']:
            with pytest.raises(ParseError):
                parse(expression)

        for expression in [')(', '))((', ')))(((']:
            with pytest.raises(ParseError):
                parse(expression)

        for expression in ['()', '()()', '()()()']:
            with pytest.raises(ParseError):
                parse(expression)

    @pytest.mark.xfail(reason='IndexError indicates parsing problem')
    def test_wrong_braces_1(self):
        parse = BooleanAlgebra().parse

        for s in ['(a', 'a(', '((a', '(a(', 'a((']:
            with pytest.raises(ParseError):
                parse(s)

        for s in ['a)', ')a', '))a', ')a)', 'a))']:
            with pytest.raises(ParseError):
                parse(s)

        for s in ['a()', '()a', 'a(())', '(a())', '(()a)', '(())a']:
            with pytest.raises(ParseError):
                parse(s)

        for s in ['a)(', ')a(', ')(a']:
            with pytest.raises(ParseError):
                parse(s)

        for s in ['a))((', ')a)((', '))(a(', '))((a']:
            with pytest.raises(ParseError):
                parse(s)

        for s in ['(a)()', '()(a)', '(a)(a)']:
            with pytest.raises(ParseError):
                parse(s)

    def test_one_symbol_0(self):
        algebra = BooleanAlgebra()

        a = algebra.Symbol('a')

        parse = algebra.parse

        assert parse('a') == a
        assert parse('(a)') == a
        assert parse('((a))') == a
        assert parse('(((a)))') == a

        for neg in ['~', '!']:
            for space in ['', ' ', '  ']:
                assert parse(neg + space + 'a') == ~a
                assert parse(neg + space + '(a)') == ~a
                assert parse(neg + space + '((a))') == ~a

                assert parse('(' + neg + space + 'a)') == ~a
                assert parse('(' + neg + space + '(a))') == ~a
                assert parse('((' + neg + space + 'a))') == ~a

        # test `not` separately because it needs extra space
        assert parse('not a') == ~a

        assert parse('not(a)') == ~a
        assert parse('not (a)') == ~a

        assert parse('not((a))') == ~a
        assert parse('not ((a))') == ~a

        assert parse('(not(a))') == ~a
        assert parse('(not (a))') == ~a

        assert parse('((not a))') == ~a

    def test_one_symbol_1(self):
        algebra = BooleanAlgebra()

        for neg in ['~', '!', 'not']:
            with pytest.raises(ParseError):
                algebra.parse(neg)

    def test_one_symbol_2(self):
        algebra = BooleanAlgebra()

        for x in ['&', 'and', '*']:
            with pytest.raises(ParseError):
                algebra.parse(x)

        for x in ['|', 'or', '+']:
            with pytest.raises(ParseError):
                algebra.parse(x)

    def test_one_symbol_3(self):
        algebra = BooleanAlgebra()

        a = algebra.Symbol('a')

        invalids = [
            'a a',
            'a a a',
            'a not',
            'a!',
            'a~',
            'a !',
            'a ~',
            'a not a',
            'a!a',
            'a! a',
            'a !a',
            'a ! a',
            'a~a',
            'a~ a',
            'a ~a',
            'a ~ a',
            'not a a',
            '!a a',
            '! a a',
            '~a a',
            '~ a a',
            'a not not a',
            'a!!a',
            'a!! a',
            'a !!a',
            'a !! a',
            'a ! ! a',
            'a! ! a',
            'a ! !a',
        ]

        for invalid in invalids:
            with pytest.raises(ParseError):
                algebra.parse(invalid)
