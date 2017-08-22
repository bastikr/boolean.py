let assert = require('assert')

let boolean = require('../boolean.js/__javascript__/boolean.js')

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
