.. testsetup:: boolean

    from boolean import *

===========
User Guide
===========

This document provides an introduction on **boolean.py** usage. It
requires that you are already familiar with Python and know a little bit
about boolean algebra. All definitions and laws are stated in :doc:`concepts`.

.. contents::
    :depth: 2
    :backlinks: top

Introduction
------------

**boolean.py** implements a boolean algebra. It
defines two base elements, *TRUE* and *FALSE*, and a class :class:`Symbol` for variables.
Expressions are built by composing symbols and elements with AND, OR and NOT.
Other compositions like XOR and NAND are not implemented.


Installation
------------
::
    pip install boolean.py


Creating boolean expressions
----------------------------

There are three ways to create a boolean expression. They all start by creating
an algebra, then use algebra attributes and methods to build expressions.


You can build an expression from a string:

.. doctest:: boolean
    
    >>> import boolean
    >>> algebra = boolean.BooleanAlgebra()
    >>> algebra.parse('x & y')
    AND(Symbol('x'), Symbol('y'))

    >>> parse('(apple or banana and (orange or pineapple and (lemon or cherry)))')
    OR(Symbol('apple'), AND(Symbol('banana'), OR(Symbol('orange'), AND(Symbol('pineapple'), OR(Symbol('lemon'), Symbol('cherry'))))))


You can build an expression from a Python expression:

.. doctest:: boolean

    >>> import boolean
    >>> algebra = boolean.BooleanAlgebra()
    >>> x, y = algebra.symbols('x', 'y')
    >>> x & y
    AND(Symbol('x'), Symbol('y'))

You can build an expression by using the algebra functions::

.. doctest:: boolean

    >>> import boolean
    >>> algebra = boolean.BooleanAlgebra()
    >>> x, y = algebra.symbols('x', 'y')
    >>> TRUE, FALSE, NOT, AND, OR, symbol = algebra.definition()
    >>> expr = AND(x, y, NOT(OR(symbol('a'), symbol('b'))))
    >>> expr
    AND(Symbol('x'), Symbol('y'))
    >>> print(expr.pretty())

    >>> print(expr)


Evaluation of expressions
-------------------------

By default, an expression is not evaluated. You need to call the .simplify() 
method explicitly an expression to perform some minimal 
simplification to evaluate an expression:

.. doctest:: boolean

    >>> import boolean
    >>> algebra = boolean.BooleanAlgebra()
    >>> x, y = algebra.symbols('x', 'y')
    >>> print(x&~x)
    0
    >>> print(x|~x)
    1
    >>> print(x|x)
    x
    >>> print(x&x)
    x
    >>> print(x&(x|y))
    x
    >>> print((x&y) | (x&~y))
    x

When simplify() is called, the following boolean logic laws are used recursively on every sub-term of the expression:

* :ref:`associativity`
* :ref:`annihilator`
* :ref:`idempotence`
* :ref:`identity`
* :ref:`complementation`
* :ref:`elimination`
* :ref:`absorption`
* :ref:`negative-absorption`
* :ref:`commutativity` (for sorting)

Also double negations are canceled out (:ref:`double-negation`).

A simplified expression is return and many not be fully evaluated nor minimal:

.. doctest:: boolean

    >>> import boolean
    >>> algebra = boolean.BooleanAlgebra()
    >>> x, y, z = algebra.symbols('x', 'y', 'z')
    >>> print((((x|y)&z)|x&y).simplify())
    (x&y)|(z&(x|y))


Equality of expressions
-----------------------

The expressions equality is tested by the :meth:`__eq__` method and therefore 
the output of :math:`expr_1 == expr_2` is not the same as mathematical equality. 

Two expressions are equal if their structure and symbols are equal.


Equality of Symbols
^^^^^^^^^^^^^^^^^^^

Symbols are equal if they are the same or their associated objects are equal.

.. doctest:: boolean

    >>> import boolean
    >>> algebra = boolean.BooleanAlgebra()
    >>> x, y, z = algebra.symbols('x', 'y', 'z')
    >>> x == y
    False
    >>> x1, x2 = algebra.symbols("x", "x")
    >>> x1 == x2
    True
    >>> x1, x2 = algebra.symbols(10, 10)
    >>> x1 == x2
    True

Equality of structure
^^^^^^^^^^^^^^^^^^^^^

Here some examples of equal and unequal structures:

.. doctest:: boolean

    >>> import boolean
    >>> algebra = boolean.BooleanAlgebra()
    >>> expr1 = algebra.parse("x|y")
    >>> expr2 = algebra.parse("y|x")
    >>> expr1 == expr2
    True
    >>> expr = algebra.parse("x|~x")
    >>> expr == TRUE
    False
    >>> expr1 = algebra.parse("x&(~x|y)")
    >>> expr2 = algebra.parse("x&y")
    >>> expr1 == expr2
    False


Analyzing a boolean expression
------------------------------

Getting sub-terms
^^^^^^^^^^^^^^^^^

All expressions have a property :attr:`args` which is a tuple of its terms.
For symbols and base elements this tuple is empty, for boolean functions it 
contains one or more symbols, elements or sub-expressions.
::

    >>> import boolean
    >>> algebra = boolean.BooleanAlgebra()
    >>> algebra.parse("x|y|z").args
    (Symbol('x'), Symbol('y'), Symbol('z'))

Getting all symbols
^^^^^^^^^^^^^^^^^^^

To get a set() of all unique symbols in an expression, use its :attr:`symbols` attribute ::

    >>> import boolean
    >>> algebra = boolean.BooleanAlgebra()
    >>> algebra.parse("x|y&(x|z)").symbols
    {Symbol('y'), Symbol('x'), Symbol('z')}

To get a list of all symbols in an expression, use its :attr:`get_symbols` method ::

    >>> import boolean
    >>> algebra = boolean.BooleanAlgebra()
    >>> algebra.parse("x|y&(x|z)").get_symbols()
    [Symbol('x'), Symbol('y'), Symbol('x'), Symbol('z')]


Literals
^^^^^^^^

Symbols and negations of symbols are called literals. You can test if an expression is a literal::

    >>> import boolean
    >>> algebra = boolean.BooleanAlgebra()
    >>> x, y, z = algebra.symbols('x', 'y', 'z')
    >>> x.isliteral
    True
    >>> (~x).isliteral
    True
    >>> (x|y).isliteral
    False

Or get a set() or list of all literals contained in an expression::

    >>> import boolean
    >>> algebra = boolean.BooleanAlgebra()
    >>> x, y, z = algebra.symbols('x', 'y', 'z')
    >>> x.literals
    {Symbol('x')}
    >>> (~(x|~y)).get_literals()
    [Symbol('x'), NOT(Symbol('y'))]

To remove negations except in literals use the :meth:`literalize`::

    >>> (~(x|~y)).literalize()
    ~x&y


Substitutions
^^^^^^^^^^^^^

To substitute parts of an expression, use the :meth:`subs` method::

    >>> e = x|y&z
    >>> e.subs({y&z:y})
    x|y


Using boolean.py to define your own boolean algebra
---------------------------------------------------

You can customize about everything in boolean.py to create your own custom algebra:
1. You can subclass :class:`BooleanAlgebra` and override or extend the 
:meth:`tokenize`:: and :meth:`parse`:: method to parse custom expression creating
your own mini expression language. Seen the tests for examples.

2. You can subclass the Symbol, NOT, AND and OR functions to add additional 
methods or for custom representations.
When doing so, you configure  :class:`BooleanAlgebra` instances by passing the custom sub-classes as agruments. 
