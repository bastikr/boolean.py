.. testsetup:: boolean

    from boolean import *

===========
Users Guide
===========

This document gives an easy introduction into **boolean.py**. It
requires that you are already familiar with python and know a little bit
about boolean algebra. All used definitions and laws are stated in
:doc:`concepts`.

.. contents::
    :depth: 2
    :backlinks: top

Introduction
------------

**boolean.py** is a single python module implementing a boolean algebra. It
defines two base elements, *TRUE* and *FALSE*, and a class :class:`Symbol`
which can take on one of these two values. Calculations are done in terms
of AND, OR and NOT - other compositions like XOR and NAND are not implemented.


Installation
------------

Locally from a checkout or download::
    pip install .

Remotely::
    pip install https://github.com/bastikr/boolean.py/archive/master.zip


Then import it:

.. doctest:: boolean

    >>> import boolean

Creating boolean expressions
----------------------------

There are many ways to create a specific boolean expression. To define boolean
variables you can directly call the Symbol class:

.. doctest:: boolean

    >>> x = Symbol("x")
    >>> y = Symbol("y")
    >>> z = Symbol("z")

A shortcut for this would be:

.. doctest:: boolean

    >>> x, y, z = symbols("x", "y", "z")

Alternatively anonymous symbols can be created by simply not giving any
argument (or :keyword:`None`) to Symbol:

.. doctest:: boolean

    >>> u = Symbol()

These defined Symbols can be composed in different ways:

.. doctest:: boolean

    >>> AND(x, y)
    AND(Symbol('x'), Symbol('y'))
    >>> x*y
    AND(Symbol('x'), Symbol('y'))
    >>> OR(NOT(y), x)
    OR(Symbol('x'), NOT(Symbol('y')))
    >>> x + ~y
    OR(Symbol('x'), NOT(Symbol('y')))

The output above maybe seems to be a little long, but this is only the result
of :func:`repr`. Printing looks a lot nicer:

.. doctest:: boolean

    >>> print(x+y)
    x+y

Yet another possibility is to parse a string into a boolean expression:

.. doctest:: boolean

    >>> print(parse("x+y"))
    x+y

    >>> parse('(apple + banana * (orange + anana * (lemon + cherry)))')
    OR(Symbol('apple'), AND(Symbol('banana'), OR(Symbol('orange'), AND(Symbol('anana'), OR(Symbol('cherry'), Symbol('lemon'))))))

.. note::

    When using :func:`parse` you don't have to define every symbol separately
    and therefore you can save a bit of typing. This is especially useful when
    using **boolean.py** interactively.


Evaluation of expressions
-------------------------

By default, all entered expressions are evaluated - that means some cheap
simplifications are carried out and then the result is returned:

.. doctest:: boolean

    >>> print(x*~x)
    0
    >>> print(x+~x)
    1
    >>> print(x+x)
    x
    >>> print(x*x)
    x
    >>> print(x*(x+y))
    x
    >>> print((x*y) + (x*~y))
    x

In detail the following laws are used recursively on every sub-term of +
and \*:

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

Be aware that you can still have nested expressions:

.. doctest:: boolean

    >>> print(((x+y)*z)+x*y)
    (x*y)+(z*(x+y))

If this automatic evaluation is unwanted, the keyword *simplify* can be used:

.. doctest:: boolean

    >>> print(AND(x, NOT(x), simplify=False))
    x*~x

Since it can be very tedious to write *simplify*\=\ :keyword:`False` and the
class-names instead of the abbreviations * and + for every operation, it can
be much easier to use the function *parse* instead:

.. doctest:: boolean

    >>> print(parse("x*~x", simplify=False))
    x*~x


Equality of expressions
-----------------------

The equality tested by the :meth:`__eq__` method and therefore the output of
:math:`expr_1 == expr_2` is not the same as mathematical equality. It simply
would be too expensive to calculate mathematical equality in many cases.
Instead two expressions are defined to be equal if the structure of the
expressions and the used symbols are equal.

Equality of Symbols
^^^^^^^^^^^^^^^^^^^

First it's important to know that Symbols
can be constructed in two different ways:

#. Anonymous symbols: Without argument or :keyword:`None`.

#. Named symbols: With any object.

Two anonymous symbols are only equal if they are the same object and can
never be equal to a named symbol:

.. doctest:: boolean

    >>> x, y, z = symbols(None, None, "z")
    >>> x == y
    False
    >>> x == x
    True
    >>> x == z
    False

Two named symbols are equal if they are the same or their associated objects
compare to equal:

.. doctest:: boolean

    >>> x, y, z = symbols("x", "y", "z")
    >>> x == y
    False
    >>> x1, x2 = symbols("x", "x")
    >>> x1 == x2
    True
    >>> x1, x2 = symbols(10, 10)
    >>> x1 == x2
    True

Equality of structure
^^^^^^^^^^^^^^^^^^^^^

Here some examples of equal and unequal structures:

.. doctest:: boolean

    >>> expr1 = parse("x+y", simplify=False)
    >>> expr2 = parse("y+x", simplify=False)
    >>> expr1 == expr2
    True
    >>> expr = parse("x+~x", simplify=False)
    >>> expr == TRUE
    False
    >>> expr1 = parse("x*(~x+y)", simplify=False)
    >>> expr2 = parse("x*y", simplify=False)
    >>> expr1 == expr2
    False


Analyzing a boolean expression
------------------------------

Getting sub-terms
^^^^^^^^^^^^^^^^^

All expressions have a property :attr:`args` which holds a tuple of sub-terms.
For symbols and base elements this tuple is empty, for boolean functions it is
holding the single terms, etc. ::

    >>> parse("x+y+z").args
    (Symbol('x'), Symbol('y'), Symbol('z'))

Getting all symbols
^^^^^^^^^^^^^^^^^^^

To get all symbols in an expression, simply use its :attr:`symbol` attribute ::

    >>> parse("x+y*(x+z)").symbols
    {Symbol('y'), Symbol('x'), Symbol('z')}


Literals
^^^^^^^^

Symbols and negations of symbols are called literals. There are several ways
to work with them. An expression can be tested if it's a literal::

    >>> x.isliteral
    True
    >>> (~x).isliteral
    True
    >>> (x+y).isliteral
    False

Or all literals contained in an expression can be obtained::

    >>> x.literals
    {Symbol('x')}
    >>> (~(x+~y)).literals
    {Symbol('x'), NOT(Symbol('y'))}

To have negations only in literals and no negations of other expressions,
:meth:`literalize` can be used::

    >>> (~(x+~y)).literalize()
    ~x*y


Substitutions
^^^^^^^^^^^^^

To substitute parts of an expression, the :meth:`subs` method can be used::

    >>> e = x+y*z
    >>> e.subs({y*z:y})
    x+y

Using boolean.py to define your own boolean algebra
---------------------------------------------------

The usage of boolean.py by its own is pretty limited. However, sometimes
boolean algebras occur in completely different programming tasks. Here a small
example shows how to implement filters which can be mixed according to boolean
algebra.
Let's define some basic interface which all specific filters should inherit
from::


    import boolean

    class Filter(boolean.BooleanAlgebra):
        def __init__(self, *, bool_expr=None):
            boolean.BooleanBase.__init__(self, bool_expr=bool_expr,
                                            bool_base=Filter)

        def simplify(self, *args, **kwargs):
            subs_dict = {}
            for h in self.bool_expr.holders:
                subs_dict[h.bool_expr] = h.simplify(*args, **kwargs)
            return self.bool_expr.subs(subs_dict)


