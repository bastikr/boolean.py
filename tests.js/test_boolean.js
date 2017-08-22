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
