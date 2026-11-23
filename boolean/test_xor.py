import itertools

import pytest

import boolean


@pytest.mark.parametrize("text", ["a ^ b", "a xor b", "a XOR b", "a XoR b"])
def test_parse_and_evaluate_xor(text):
    algebra = boolean.BooleanAlgebra()
    expression = algebra.parse(text)
    assert isinstance(expression, algebra.XOR)
    assert str(expression) == "a^b"
    for a, b in itertools.product((False, True), repeat=2):
        assert expression(a=a, b=b) == (a != b)


def test_xor_precedence_and_python_operator():
    algebra = boolean.BooleanAlgebra()
    a, b, c, d = algebra.symbols("a", "b", "c", "d")
    assert algebra.parse("~a & b ^ c | d") == (((~a & b) ^ c) | d)
    assert algebra.parse("a ^ (b | c)") == (a ^ (b | c))
    assert algebra.parse("a ^ b ^ c") == algebra.XOR(a, b, c)
    assert algebra.parse("xor_value & xordinary").symbols == set(
        algebra.symbols("xor_value", "xordinary")
    )
    assert len(algebra.definition()) == 6


@pytest.mark.parametrize(
    "text", ["^ a", "a ^", "a ^ ^ b", "a ^ | b", "(^ a)", "a ^ ()"]
)
def test_xor_parse_errors(text):
    with pytest.raises(boolean.ParseError):
        boolean.BooleanAlgebra().parse(text)


@pytest.mark.parametrize(
    "source, expected",
    [
        ("a ^ a", "0"),
        ("a ^ a ^ a", "a"),
        ("a ^ 0", "a"),
        ("a ^ 1", "~a"),
        ("a ^ ~a", "1"),
        ("a ^ ~a ^ b", "~b"),
        ("~a ^ ~b", "a ^ b"),
        ("(a ^ b) ^ (b ^ c)", "a ^ c"),
        ("(a & b) ^ (b & a)", "0"),
        ("~(a ^ a)", "1"),
        ("~(a ^ 1)", "a"),
    ],
)
def test_xor_simplification(source, expected):
    algebra = boolean.BooleanAlgebra()
    result = algebra.parse(source).simplify()
    assert result == algebra.parse(expected).simplify()
    assert result.simplify() == result


def test_xor_structural_equality_preserves_multiplicity():
    algebra = boolean.BooleanAlgebra()
    a, b = algebra.symbols("a", "b")
    assert algebra.XOR(a, a, b) != algebra.XOR(a, b)
    assert algebra.XOR(a, a, b) == algebra.XOR(b, a, a)
    assert hash(algebra.XOR(a, a, b)) == hash(algebra.XOR(b, a, a))
    assert len({algebra.XOR(a, a, b), algebra.XOR(a, b)}) == 2
    assert algebra.XOR(a, a, b).subs({a: algebra.TRUE}, simplify=True) == b


@pytest.mark.parametrize(
    "text",
    [
        "a ^ b ^ c",
        "~(a ^ (b & c))",
        "(a ^ b) & (~a | c)",
        "(a ^ b) | (c ^ d)",
        "~(a ^ b ^ c ^ d)",
        "(a ^ a) ^ (b ^ b)",
        "a ^ 1",
        "1 ^ 0 ^ 1",
    ],
)
def test_xor_normal_forms_preserve_the_truth_table(text):
    algebra = boolean.BooleanAlgebra()
    expression = algebra.parse(text)
    variants = [
        expression.simplify(),
        expression.literalize(),
        algebra.cnf(expression),
        algebra.dnf(expression),
    ]
    for normal in variants[-2:]:
        assert "XOR" not in repr(normal)
    for values in itertools.product((False, True), repeat=4):
        inputs = dict(zip("abcd", values))
        expected = expression(**inputs)
        assert all(bool(variant(**inputs)) == expected for variant in variants)


def test_xor_nary_parity_cancels_without_expansion():
    algebra = boolean.BooleanAlgebra()
    symbols = algebra.symbols(*range(1000))
    assert algebra.XOR(*(symbols + symbols)).simplify() == algebra.FALSE
    assert algebra.cnf(algebra.XOR(*(symbols + symbols))) == algebra.FALSE
    assert algebra.dnf(algebra.XOR(*(symbols + symbols))) == algebra.FALSE
    expression = algebra.XOR(*symbols).simplify()
    assert isinstance(expression, algebra.XOR)
    assert len(expression.args) == 1000


def test_xor_uses_custom_algebra_classes():
    class CustomXor(boolean.XOR):
        pass

    class CustomSymbol(boolean.Symbol):
        pass

    try:
        algebra = boolean.BooleanAlgebra(XOR_class=CustomXor, Symbol_class=CustomSymbol)
        a, b = algebra.symbols("a", "b")
        assert isinstance(a ^ b, CustomXor)
        assert isinstance(algebra.parse("a ^ b"), CustomXor)
        assert all(isinstance(s, CustomSymbol) for s in algebra.cnf(a ^ b).symbols)
    finally:
        boolean.BooleanAlgebra()
