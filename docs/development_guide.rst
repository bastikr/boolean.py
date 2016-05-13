=================
Development Guide
=================

This document gives an overview of the code in &&boolean.py&&, explaining the
layout and design decisions and some difficult algorithms. All used definitions
and laws are stated in :doc:`concepts`.

.. contents::
    :depth: 2
    :backlinks: top

Classes Hierarchy
-----------------
..
    boolean.boolean.BooleanAlgebra
    boolean.boolean.Expression
        boolean.boolean.BaseElement
            boolean.boolean._TRUE
            boolean.boolean._FALSE
        boolean.boolean.Symbol
        boolean.boolean.Function
            boolean.boolean.NOT
            boolean.boolean.DualBase
                boolean.boolean.AND
                boolean.boolean.OR



Expression
^^^^^^^^^^
..
    .. autoclass:: boolean.boolean.Expression

Symbol
^^^^^^
..
    .. autoclass:: boolean.boolean.Symbol

Function
^^^^^^^^
..
    .. autoclass:: boolean.boolean.Function

NOT
^^^
..
    .. autoclass:: boolean.boolean.NOT

AND
^^^
..
    .. autoclass:: boolean.boolean.AND

OR
^^
..
    .. autoclass:: boolean.boolean.OR


.. _class-creation:

Class creation
--------------

Except for BooleanAlgebra and Symbol, no other classes are is designed to be instantiated directly.
Instead you should create a BooleanAlgebra instance, then use  BooleanAlgebra.symbol, 
BooleanAlgebra.NOT, BooleanAlgebra.AND, BooleanAlgebra.OR BooleanAlgebra.TRUE and BooleanAlgebra.FALSE 
to compose your expressions in the context of this algebra.


.. _class-initialization:

Class initialization
--------------------

In this section for all classes is stated which arguments they will accept
and how these arguments are processed before they are used.

Symbol
^^^^^^

& :obj:`obj` (Named Symbol)


Ordering
--------

As far as possible every expression should always be printed in exactly the
same way. Therefore a strict ordering between different boolean classes and
between instances of same classes is needed. This is defined primarily by the
sort_order attribute.


Class ordering
^^^^^^^^^^^^^^

:class:`BaseElement` < :class:`Symbol` < :class:`AND` <  :class:`OR`

:class:`NOT` is an exception in this scheme. It will be sorted based on the sort order of its
argument.

Class ordering is implemented by an attribute :attr:`sort_order` in all
relevant classes. It holds an integer that will be used for comparison
if it is available in both compared objects.
For Symbols, the attached `obj` object is used instead.
 
+----------------------+-----------+
|    :class:`Class`    | sort_order|
+======================+===========+
| :class:`BaseElement` |    0      |
+----------------------+-----------+
| :class:`Symbol`      |    5      |
+----------------------+-----------+
| :class:`AND`         |    10     |
+----------------------+-----------+
| :class:`OR`          |    25     |
+----------------------+-----------+

Instance ordering
^^^^^^^^^^^^^^^^^

:class:`BaseElement`
    :obj:`FALSE` < :obj:`TRUE`

:class:`Symbol`

    :obj:`Symbol.obj` o  :obj:`Symbol.obj`

:class:`NOT`
    if :obj:`NOT.args[0]` == :obj:`other` ---> :obj:`other` < :obj:`NOT`

    :obj:`NOT` o :obj:`other` ---> :obj:`NOT.args[0]` o :obj:`other`

:class:`AND`
    :obj:`AND` o :obj:`AND` ---> :obj:`AND.args[0]` o :obj:`AND.args[0]`

    if undecided: repeat for all args

    if undecided: len(:obj:`AND.args`) o len(:obj:`AND.args`)

    if undecided: return :obj:`AND` < :obj:`AND`

:class:`OR`
    :obj:`OR` o :obj:`OR` ---> :obj:`OR.args[0]` o :obj:`OR.args[0]`

    if undecided: repeat for all args

    if undecided: len(:obj:`OR.args`) o len(:obj:`OR.args`)

    if undecided: return :obj:`OR` < :obj:`OR`


Parsing
-------

Parsing is done in two steps:
A tokenizer iterates over string characters assigning a TOKEN_TYPE to each token.
The parser receives this stream of token types and strings and creates
adequate boolean objects from a parse tree.

