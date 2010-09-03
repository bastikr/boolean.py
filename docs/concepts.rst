========================
Concepts and Definitions
========================

In this document the basic definitions and importent laws of Boolean algebra
are stated.

.. contents::
    :depth: 2
    :backlinks: top

Basic Definitions
-----------------


Set of Base Elements
^^^^^^^^^^^^^^^^^^^^

S := {1, 0}

*These base elements are own classes with only one instance each,
called* :class:`TRUE` *and* :class:`FALSE`.


Boolean Variable
^^^^^^^^^^^^^^^^

A variable that takes on only values in S.

*Implemented as class* :class:`Symbol`.


Boolean Function
^^^^^^^^^^^^^^^^

A function :math:`f: S^n \rightarrow S` (where n is called the order).

*Implemented as class* :class:`Function`.


Boolean Expression
^^^^^^^^^^^^^^^^^^

Either a base element, a boolean variable or a boolean function.

*Implemented as class* :class:`Expression` *- this is the base class
for* :class:`BaseElement`, :class:`Symbol` *and* :class:`Function`.

NOT
^^^

A boolean function of order 1 with following truthtable:

+---+--------+
| x | NOT(x) |
+===+========+
| 0 |   1    |
+---+--------+
| 1 |   0    |
+---+--------+

Instead of :math:`NOT(x)` one can write :math:`~x`.

*Implemented as class* :class:`NOT`.


AND
^^^

A boolean function of order 2 or more with the truthtable for two
elements

+---+---+----------+
| x | y | AND(x,y) |
+===+===+==========+
| 0 | 0 |    0     |
+---+---+----------+
| 0 | 1 |    0     |
+---+---+----------+
| 1 | 0 |    0     |
+---+---+----------+
| 1 | 1 |    1     |
+---+---+----------+

and the property :math:`AND(x, y, z) = AND(x, AND(y, z))` where
:math:`x, y, z` are boolean expressions.

Instead of :math:`AND(x, y, z)` one can write :math:`x*y*z`.

*Implemented as class* :class:`AND`.


OR
^^

A boolean function of order 2 or more with the truthtable for two
elements 

+---+---+---------+
| x | y | OR(x,y) |
+===+===+=========+
| 0 | 0 |    0    |
+---+---+---------+
| 0 | 1 |    0    |
+---+---+---------+
| 1 | 0 |    0    |
+---+---+---------+
| 1 | 1 |    1    |
+---+---+---------+

and the property :math:`OR(x, y, z) = OR(x, OR(y, z))` where
:math:`x, y, z` are boolean expressions.

Instead of :math:`OR(x, y, z)` one can write :math:`x+y+z`.

*Implemented as class* :class:`OR`.


Literal
^^^^^^^

A boolean variable or it's negation.

*Implemented indirectly as* :attr:`Expression.isliteral`,
:attr:`Expression.literals` *and* :meth:`Expression.literalize`.

Disjunctive normal form (DNF)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A disjunction of conjunctions of literals where the conjunctions don't
contain a boolean variable *and* it's negation. An example would be
:math:`x*y + x*z`.

*Implemented as class* :class:`DNF`.


Full disjunctive normal form (FDNF)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A DNF where all conjunctions have the same count of literals as the
whole DNF has boolean variables. An example would be
:math:`x*y*z + x*y*~z + x*~y*z`.


Conjunctive normal form (CNF)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A conjunction of disjunctions of literals where the disjunctions don't
contain a boolean variable *and* it's negation. An example would be
:math:`(x+y) * (x+z)`.

*Implemented as class* :class:`CNF`.

Full conjunctive normal form (FCNF)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A CNF where all disjunctions have the same count of literals as the
whole CNF has boolean variables. An example would be:
:math:`(x+y+z) * (x+y+~z) * (x+~y+z)`.

Laws
----

In this section different laws are listed that are directly derived from the
definitions stated above.

In the following :math:`x, y, z` are boolean expressions.

.. _associativity:

Associativity
^^^^^^^^^^^^^

* :math:`x*(y*z) = (x*y)*z`
* :math:`x+(y+z) = (x+y)+z`


.. _commutativity:

Commutativity
^^^^^^^^^^^^^

* :math:`x*y = y*x`
* :math:`x+y = y+x`


.. _distributivity:

Distributivity
^^^^^^^^^^^^^^

* :math:`x*(y+z) = (x*y)+(x*z)`
* :math:`x+(y*z) = (x+y)*(x+z)`


.. _identity:

Identity
^^^^^^^^

* :math:`x*1 = x`
* :math:`x+0 = x`


.. _annihilator:

Annihilator
^^^^^^^^^^^

* :math:`x*0 = 0`
* :math:`x+1 = 1`


.. _idempotence:

Idempotence
^^^^^^^^^^^

* :math:`x*x = x`
* :math:`x+x = x`


.. _absorption:

Absorption
^^^^^^^^^^

* :math:`x*(x+y) = x`
* :math:`x+(x*y) = x`


.. _negative-absorption:

Negative absorption
^^^^^^^^^^^^^^^^^^^

* :math:`x*(~x+y) = x*y`
* :math:`x+(~x*y) = x+y`


.. _complementation:

Complementation
^^^^^^^^^^^^^^^

* :math:`x*~x = 0`
* :math:`x+~x = 1`


.. _double-negation:

Double negation
^^^^^^^^^^^^^^^

* :math:`~~x = x`


.. _de-morgan:

De Morgan
^^^^^^^^^

* :math:`~(x*y) = ~x + ~y`
* :math:`~(x+y) = ~x * ~y`


.. _elemination:

Elemination
^^^^^^^^^^^

* :math:`(x*y) + (x*~y) = x`
* :math:`(x+y) * (x+~y) = x`
