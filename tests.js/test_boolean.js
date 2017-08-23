let assert = require('assert')

let boolean = require('../boolean.js/__javascript__/boolean.js')

let Symbol = boolean.boolean.Symbol
let BooleanAlgebra = boolean.boolean.BooleanAlgebra

describe('BaseElement', function() {
    let algebra

    beforeEach(function() {
        algebra = BooleanAlgebra()
    })

    it('TRUE and FALSE make sense', function() {
        assert.equal(algebra.TRUE, algebra.TRUE)
        assert.equal(algebra.FALSE, algebra.FALSE)

        assert.ok(algebra.TRUE === algebra.TRUE)
        assert.ok(algebra.FALSE === algebra.FALSE)

        assert.notEqual(algebra.TRUE, algebra.FALSE)
        assert.notEqual(algebra.FALSE, algebra.TRUE)

        assert.ok(algebra.TRUE != algebra.FALSE)
        assert.ok(algebra.FALSE != algebra.TRUE)
    })

    it('literals are empty sets', function() {
        // algebra.TRUE.literals and algebra.FALSE.literals are weird objects
        assert.ok(algebra.TRUE .literals().__class__.__name__ === 'set')
        assert.ok(algebra.FALSE.literals().__class__.__name__ === 'set')
    })

    it('.literalize() works on basics', function() {
        assert.equal(algebra.TRUE.literalize(), algebra.TRUE)
        assert.equal(algebra.FALSE.literalize(), algebra.FALSE)

        assert.notEqual(algebra.TRUE.literalize(), algebra.FALSE)
        assert.notEqual(algebra.FALSE.literalize(), algebra.TRUE)
    })

    it('.simplify() works on basics', function() {
        assert.equal(algebra.TRUE.simplify(), algebra.TRUE)
        assert.equal(algebra.FALSE.simplify(), algebra.FALSE)

        assert.notEqual(algebra.TRUE.simplify(), algebra.FALSE)
        assert.notEqual(algebra.FALSE.simplify(), algebra.TRUE)
    })

    it('dual works on basics', function() {
        assert.equal(algebra.TRUE.dual, algebra.FALSE)
        assert.equal(algebra.FALSE.dual, algebra.TRUE)
    })

    it('order works on basics', function() {
        assert.ok(algebra.FALSE < algebra.TRUE)
        assert.ok(algebra.TRUE > algebra.FALSE)
    })

    it('converting to string makes sense', function() {
        assert.ok(algebra.FALSE.toString() === '0')
        assert.ok(algebra.TRUE .toString() === '1')

        assert.ok(algebra.FALSE.__repr__() === 'FALSE')
        assert.ok(algebra.TRUE .__repr__() === 'TRUE' )
    })
})

describe('Symbol', function() {
    let symbol, symbol1, symbol2, same0, same1

    beforeEach(function() {
        symbol0 = Symbol('string as a symbol')

        symbol1 = Symbol(1)
        symbol2 = Symbol(2)

        same0 = Symbol('sibling symbol')
        same1 = Symbol('sibling symbol')
    })

    it('isliteral is true by default', function() {
        assert.equal(symbol1.isliteral, true)
    })

    it('symbol contains itself in .literals', function() {
        assert.ok(symbol0.literals().indexOf(symbol0) !== -1)
    })

    it('symbols with same obj compare equal', function() {
        assert.ok(same0.__eq__(same1))
        assert.equal(same0.obj, same1.obj)

        // Javascript will not let you overload !=, let alone !==
        assert.ok(same0 != same1)
        assert.notEqual(same0, same1)
    })

    it('.literalize() a symbol gives that symbol', function() {
        assert.ok(symbol0.literalize() == symbol0)
        assert.ok(symbol0.literalize() === symbol0)

        assert.ok(symbol1.literalize() == symbol1)
        assert.ok(symbol1.literalize() === symbol1)

        assert.ok(symbol0.literalize() != symbol1)
        assert.ok(symbol1.literalize() != symbol0)
    })

    it('.simplify() a symbol gives that symbol', function() {
        assert.ok(symbol0.simplify() == symbol0)
        assert.ok(symbol0.simplify() === symbol0)

        assert.ok(symbol0.simplify() != symbol1)
        assert.ok(symbol1.simplify() != symbol0)
    })
})

describe('BooleanAlgebra', function() {
    let algebra, expressions, variables

    beforeEach(function() {
        algebra = BooleanAlgebra()
        variables = ['a', 'b', 'c', 'd', 'e', 'f']
    })

    it('parse a single variable', function() {
        expression = algebra.parse('a')

        assert.ok(expression.__name__ === 'Symbol')
        assert.ok(expression.obj === 'a')
    })

    expressions = [
        'a or b', 'a OR b', 'a | b', 'a || b', 'a oR b', 'a oR b'
    ]
    for (let expression of expressions) {
        it('parse ' + expression, function() {
            expression = algebra.parse(expression)

            assert.ok(expression.__name__ === 'OR')
            assert.equal(expression.args.length, 2)

            let fst = expression.args[0], snd = expression.args[1]

            assert.equal(fst.__name__, 'Symbol')
            assert.equal(snd.__name__, 'Symbol')

            assert.equal(fst.obj, 'a')
            assert.equal(snd.obj, 'b')
        })
    }

    expressions = [
        'a and b', 'a AND b', 'a & b', 'a && b', 'a aND b', 'a aNd b'
    ]
    for (let expression of expressions) {
        it('parse ' + expression, function() {
            expression = algebra.parse(expression)

            assert.ok(expression.__name__ === 'AND')
            assert.equal(expression.args.length, 2)

            let fst = expression.args[0], snd = expression.args[1]

            assert.equal(fst.__name__, 'Symbol')
            assert.equal(snd.__name__, 'Symbol')

            assert.equal(fst.obj, 'a')
            assert.equal(snd.obj, 'b')
        })
    }

    expressions = ['not a', '~a', '!a', 'nOt a', 'nOT a']
    for (let expression of expressions) {
        it('parse ' + expression, function() {
            expression = algebra.parse(expression)

            assert.ok(expression.__name__ === 'NOT')
            assert.equal(expression.args.length, 1)

            assert.equal(expression.args[0].obj, 'a')
        })
    }

    it.skip('parse empty parenthesis', function() {
        expression = algebra.parse('()')
    })

    it('parse (a)', function() {
        expression = algebra.parse('(a)')

        assert.equal(expression.obj, 'a')
    })

    it('parse (a or b)', function() {
        expression = algebra.parse('(a or b)')

        assert.equal(expression.__name__, 'OR')
        assert.equal(expression.args.length, 2)

        assert.equal(expression.args[0], 'a')
        assert.equal(expression.args[1], 'b')
    })

    it('parse (a and b)', function() {
        expression = algebra.parse('(a and b)')

        assert.equal(expression.__name__, 'AND')
        assert.equal(expression.args.length, 2)

        assert.equal(expression.args[0].obj, 'a')
        assert.equal(expression.args[1].obj, 'b')
    })

    it('parse (not a)', function() {
        expression = algebra.parse('(not a)')

        assert.equal(expression.__name__, 'NOT')
        assert.equal(expression.args.length, 1)

        assert.equal(expression.args[0].obj, 'a')
    })

    expressions = ['not (a)', '!(a)', '! (a)', '~(a)', '~  (a)']
    for (let expression of expressions) {
        it('parse ' + expression, function() {
            expression = algebra.parse(expression)

            assert.equal(expression.__name__, 'NOT')
            assert.equal(expression.args.length, 1)

            assert.equal(expression.args[0].obj, 'a')
        })
    }

    expressions = [
        'not a & not b', '~a & ~b', '!a & !b',
        'not a && not b', '~a && ~b', '!a && !b',
        'not a and not b', '~a and ~b', '!a and !b',
        'not a & not b & not c', '~a & ~b & ~c', '!a & !b & !c',
        'not a && not b && not c', '~a && ~b && ~c', '!a && !b && !c',
        'not a and not b and not c', '~a and ~b and ~c', '!a and !b and !c'
    ]
    expressions.forEach((expression, i) => {
        it.skip('parse ' + expression, function() {
            expression = algebra.parse(expression)

            assert.equal(expression.__name__, 'AND')
            if (i < 9) {
                assert.equal(expression.args.length, 2)
            } else {
                assert.equal(expression.args.length, 3)
            }
        })
    })

    expressions = [
        'not a | not b', '~a | ~b', '!a | !b',
        'not a || not b', '~a || ~b', '!a || !b',
        'not a or not b', '~a or ~b', '!a or !b',
        'not a | not b | not c', '~a | ~b | ~c', '!a | !b | !c',
        'not a || not b && not c', '~a || ~b || ~c', '!a || !b || !c',
        'not a or not b or not c', '~a or ~b or ~c', '!a or !b or !c'
    ]
    expressions.forEach((expression, i) => {
        it.skip('parse ' + expression, function() {
            expression = algebra.parse(expression)

            assert.equal(expression.__name__, 'OR')
            if (i < 9) {
                assert.equal(expression.args.length, 2)
            } else {
                assert.equal(expression.args.length, 3)
            }
        })
    })

    expressions = ['not a', '!a', '~a', 'not(a)', '(not a)', '!(a)']
    for (let expression of expressions) {
        it('literalize ' + expression, function() {
            expression = algebra.parse(expression).literalize()

            assert.equal(expression.__name__, 'NOT')
            assert.equal(expression.args.length, 1)

            assert.equal(expression.args[0].obj, 'a')
        })
    }

    expressions = [
        'not (a | b)', '~(a | b)', '!(a | b)',
        'not (a || b)', '~(a || b)', '!(a || b)',
        'not (a or b)', '~(a or b)', '!(a or b)',
        'not (a | b | c)', '~(a | b | c)', '!(a | b | c)',
        'not (a || b || c)', '~(a || b || c)', '!(a || b || c)',
        'not (a or b or c)', '~(a or b or c)', '!(a or b or c)',
        'not (a | b || c)', '~(a | b or c)', '!(a or b || c)'
    ]
    expressions.forEach((expression, i) => {
        it ('literalize ' + expression, function() {
            expression = algebra.parse(expression).literalize()

            assert.equal(expression.__name__, 'AND')
            if (i < 9) {
                assert.equal(expression.args.length, 2)
            } else {
                assert.equal(expression.args.length, 3)
            }

            for (let j = 0; j != expression.args.length; ++j) {
                assert.equal(expression.args[j].__name__, 'NOT')

                assert.equal(expression.args[j].args[0].obj, variables[j])
            }
        })
    })

    expressions = [
        'not (a & b)', '~(a & b)', '!(a & b)',
        'not (a && b)', '~(a && b)', '!(a && b)',
        'not (a and b)', '~(a and b)', '!(a and b)',
        'not (a & b & c)', '~(a & b & c)', '!(a & b & c)',
        'not (a && b && c)', '~(a && b && c)', '!(a && b && c)',
        'not (a and b and c)', '~(a and b and c)', '!(a and b and c)',
        'not (a & b && c)', '~(a & b and c)', '!(a && b and c)'
    ]
    expressions.forEach((expression, i) => {
        it('literalize ' + expression, function() {
            expression = algebra.parse(expression).literalize()

            assert.equal(expression.__name__, 'OR')
            if (i < 9) {
                assert.equal(expression.args.length, 2)
            } else {
                assert.equal(expression.args.length, 3)
            }

            for (let j = 0; j != expression.args.length; ++j) {
                assert.equal(expression.args[j].__name__, 'NOT')

                assert.equal(expression.args[j].args[0].obj, variables[j])
            }
        })
    })

    expressions = [
        '!(a and b)', '!(a & b)', '!(a && b)',
        '~(a and b)', '~(a & b)', '~(a && b)',
        'not (a and b)', 'not (a & b)', 'not (a && b)',
    ]
    expressions.forEach((expression, i) => {
        it('.demorgan() on ' + expression, function() {
            expr = algebra.parse(expression).demorgan()

            assert.equal(expr.__name__, 'OR')
            assert.equal(expr.args.length, 2)

            for (let j = 0; j != expr.args.length; ++j) {
                assert.equal(expr.args[j].__name__, 'NOT')
                assert.equal(expr.args[j].args.length, 1)

                assert.equal(expr.args[j].args[0].obj, variables[j])
            }
        })
    })

    expressions = [
        '!(a and b and c)', '!(a & b & c)', '!(a && b && c)',
        '~(a and b and c)', '~(a & b & c)', '~(a && b && c)',
        'not (a and b and c)', 'not (a & b & c)', 'not (a && b && c)'
    ]
    expressions.forEach((expression, i) => {
        it('.demorgan() on ' + expression, function() {
            expr = algebra.parse(expression).demorgan()

            assert.equal(expr.__name__, 'OR')
            assert.equal(expr.args.length, 3)

            for (let j = 0; j != expr.args.length; ++j) {
                assert.equal(expr.args[j].__name__, 'NOT')
                assert.equal(expr.args[j].args[0], variables[j])
            }
        })
    })

    expressions = [
        '!(a or b)', '!(a | b)', '!(a || b)',
        '~(a or b)', '~(a | b)', '~(a || b)',
        'not (a or b)', 'not (a | b)', 'not (a || b)'
    ]
    expressions.forEach((expression, i) => {
        it('.demorgan() on ' + expression, function() {
            expr = algebra.parse(expression).demorgan()

            assert.equal(expr.__name__, 'AND')
            assert.equal(expr.args.length, 2)

            for (let j = 0; j != expr.args.length; ++j) {
                assert.equal(expr.args[j].__name__, 'NOT')
                assert.equal(expr.args[j].args[0], variables[j])
            }
        })
    })

    expressions = [
        '!(a or b or c)', '!(a | b | c)', '!(a || b || c)',
        '~(a or b or c)', '~(a | b | c)', '!(a || b || c)',
        'not (a or b or c)', 'not (a | b | c)', 'not (a || b || c)'
    ]
    expressions.forEach((expression, i) => {
        it('.demorgan() on ' + expression, function() {
            expr = algebra.parse(expression).demorgan()

            assert.equal(expr.__name__, 'AND')
            assert.equal(expr.args.length, 3)

            for (let j = 0; j != expr.args.length; ++j) {
                assert.equal(expr.args[j].__name__, 'NOT')
                assert.equal(expr.args[j].args[0].obj, variables[j])
            }
        })
    })
})

describe('NOT', function() {
    let algebra, symbol, expressions

    beforeEach(function() {
        algebra = BooleanAlgebra()
    })

    expressions = [
        'not not a', '!!a', '~~a',
        'not !a', '! not a', '~ not a', 'not ~ a',
        'not not not not a', '!!!!a', '~~~~a'
    ]
    expressions.forEach((expression, i) => {
        it('.cancel() on ' + expression, function() {
            expr = algebra.parse(expression).cancel()

            assert.equal(expr.obj, 'a')
        })

        it.skip('.literalize() on ' + expression, function() {
            expr = algebra.parse(expression).literalize()

            assert.equal(expr.obj, 'a')
        })

        it('.simplify() on ' + expression, function() {
            expr = algebra.parse(expression).simplify()

            assert.equal(expr.obj, 'a')
        })
    })

    expressions = [
        'not a', '!a', '~a',
        'not not not a', '!!!a', '~~~a',
        'not !!a', 'not ! not a', '! not not a'
    ]
    expressions.forEach((expression, i) => {
        it('.cancel() on ' + expression, function() {
            expr = algebra.parse(expression).cancel()

            assert.equal(expr.__name__, 'NOT')
            assert.equal(expr.args.length, 1)
            assert.equal(expr.args[0].obj, 'a')
        })

        it.skip('.literalize() on ' + expression, function() {
            expr = algebra.parse(expression).literalize()

            assert.equal(expression.__name__, 'NOT')
            assert.equal(expr.args.length, 1)
            assert.equal(expr.args[0].obj, 'a')
        })

        it('.simplify() on ' + expression, function() {
            expr = algebra.parse(expression).simplify()

            assert.equal(expr.__name__, 'NOT')
            assert.equal(expr.args.length, 1)
            assert.equal(expr.args[0].obj, 'a')
        })
    })
})
