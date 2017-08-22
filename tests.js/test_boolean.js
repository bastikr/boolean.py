let assert = require('assert')

let boolean = require('../boolean.js/__javascript__/boolean.js')

let BooleanAlgebra = boolean.boolean.BooleanAlgebra

describe('BooleanAlgebra', function() {
    it('TRUE and FALSE make sense', function() {
        let algebra = BooleanAlgebra()

        assert.equal(algebra.TRUE, algebra.TRUE)
        assert.equal(algebra.FALSE, algebra.FALSE)

        assert.ok(algebra.TRUE === algebra.TRUE)
        assert.ok(algebra.FALSE === algebra.FALSE)
    })
})
