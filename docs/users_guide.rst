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

At the moment there is no installable package available - this will be done
when the module was tested a little bit more. However, it's easy enough to get
it running by downloading **boolean.py** and either adding it's location to the
*PYTHONPATH* or starting the python interpreter in the same directory.

Then **boolean.py** can be used inside python by simply importing it:

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
    OR(NOT(Symbol('y')), Symbol('x'))
    >>> x + ~y
    OR(NOT(Symbol('y')), Symbol('x'))

The output above maybe seems to be a little long, but this is only the result
of :func:`repr`. Printing looks a lot nicer:

.. doctest:: boolean

    >>> print x+y
    x+y

Yet another possibility is to parse a string into a boolean expression:

.. doctest:: boolean

    >>> print parse("x+y")
    x+y

.. note::

    When using :func:`parse` you don't have to define every symbol separately
    and therefor you can save a bit of typing. This is especially usefull when
    using **boolean.py** interactively.


Evaluation of expressions
-------------------------

By default, all entered expressions are evaluated - that means some cheap
simplifications are carried out and then the result is returned:

.. doctest:: boolean

    >>> print x*~x
    0
    >>> print x+~x
    1
    >>> print x+x
    x
    >>> print x*x
    x
    >>> print x*(x+y)
    x
    >>> print (x*y) + (x*~y)
    x

In detail the following laws are used recursively on every subterm of + and \*:

* :ref:`associativity`
* :ref:`annihilator`
* :ref:`idempotence`
* :ref:`identity`
* :ref:`complementation`
* :ref:`elemination`
* :ref:`absorption`
* :ref:`negative-absorption`
* :ref:`commutativity` (for sorting)

Also double negations are canceled out (:ref:`double-negation`).

Be aware that you can still have nested expressions:

.. doctest:: boolean

    >>> print ((x+y)*z)+x*y
    ((x+y)*z)+(x*y)

If this automatic evaluation is unwanted, the keyword *eval* can be used:

.. doctest:: boolean

    >>> print AND(x, NOT(x), eval=False)
    x*~x

Since it can be very tedious to write *eval*\=\ :keyword:`False` and the
class-names instead of the abbreviations * and + for every operation, it can
be much easier to use the function *parse* instead:

.. doctest:: boolean

    >>> print parse("x*~x", eval=False)
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

Two named symbols are equal if they are the same or their holded objects
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

    >>> expr1 = parse("x+y", eval=False)
    >>> expr2 = parse("y+x", eval=False)
    >>> expr1 == expr2
    True
    >>> expr = parse("x+~x", eval=False)
    >>> expr == TRUE
    False
    >>> expr1 = parse("x*(~x+y)", eval=False)
    >>> expr2 = parse("x*y", eval=False)
    >>> expr1 == expr2
    False



Normal Forms
------------

Using boolean.py to define your own boolean algebra
---------------------------------------------------
