=================
Development Guide
=================

This document gives an overview of the code in **boolean.py**, explaining the
layout and design decisions and some difficult algorithms. All used definitions
and laws are stated in :doc:`concepts`.

.. contents::
    :depth: 2
    :backlinks: top

Layout
------
..
    .. inheritance-diagram:: boolean.AND boolean.OR boolean.Symbol boolean.NOT
                         boolean._TRUE boolean._FALSE boolean.DNF boolean.CNF

Classes
-------

Expression
^^^^^^^^^^
..
    .. autoclass:: boolean.Expression

Symbol
^^^^^^
..
    .. autoclass:: boolean.Symbol

Function
^^^^^^^^
..
    .. autoclass:: boolean.Function

NOT
^^^
..
    .. autoclass:: boolean.NOT

AND
^^^
..
    .. autoclass:: boolean.AND

OR
^^
..
    .. autoclass:: boolean.OR


.. _class-creation:

Class creation
--------------

Not all calls to classes will result in instances of the corresponding class.
In the following it is exactly listed which classes will return different
instances for which types of arguments. So basically this section describes
all implemented :meth:`__new__` methods.

Expression
^^^^^^^^^^

The :class:`expression` class can be used to transform any reasonable object
into a boolean expression.

* :obj:`Expression`
* :obj:`str` --- :func:`parse` ---> :obj:`Expression`
* :obj:`1` | :obj:`True` ---> :obj:`TRUE`
* :obj:`0` | :obj:`False` ---> :obj:`FALSE`

BaseElement
^^^^^^^^^^^

Probably this will not be used very often but for consistency the
:class:`BaseElement` class will return the base elements of the boolean algebra
if it is called with a meaningful argument.

* :obj:`BaseElement`
* :obj:`1` | :obj:`True` ---> :obj:`TRUE`
* :obj:`0` | :obj:`False` ---> :obj:`FALSE`

.. _class-initialization:

Class initialization
--------------------

In this section for all classes is stated which arguments they will accept
(apart from the ones listed in :ref:`class-creation`) and how these arguments
are processed before they are used.

Symbol
^^^^^^

* :obj:`None` (Anonymous Symbol)
* :obj:`obj` (Named Symbol)

Function
^^^^^^^^

A function can take an amount of arguments according to its order. These
arguments may have the following types and are adequately processed:

* :obj:`Expression`
* :obj:`str` --- :func:`parse` ---> :obj:`Expression`
* :obj:`1` | :obj:`True` ---> :obj:`TRUE`
* :obj:`0` | :obj:`False` ---> :obj:`FALSE`


Ordering
--------

As far as possible every expression should always be printed in exactly the
same way. Therefor a strict ordering between different boolean classes and
between instances of same classes is needed.

Class ordering
^^^^^^^^^^^^^^

:class:`BaseElement` < :class:`Symbol` < :class:`AND` < :class:`CNF` <
:class:`FCNF` < :class:`OR` < :class:`DNF` < :class:`FDNF`

:class:`NOT` is an exception in this scheme. It will be sorted by it's
argument.

Class ordering is implemented by an attribute :attr:`cls_order` in all
relevant classes. It holds an integer that will be used for comparison
if it is in both compared classes available.

+----------------------+-----------+
|    :class:`Class`    | cls_order |
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
    :obj:`Named-Symbol` < :obj:`Anonymous-Symbol`

    :obj:`Named-Symbol` o :obj:`Named-Symbol` --->
    :obj:`Named-Symbol.arg[0]` o :obj:`Named-Symbol.arg[0]`

    :obj:`Anonymous-Symbol` o :obj:`Anonymous-Symbol` --->
    hash(:obj:`Anonymous-Symbol`) o hash(:obj:`Anonymous-Symbol`)

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

Parsing is done by iterating over all characters of a given string, creating
adequate boolean objects as soon as possible and storing them in a list until
they can be integrated into boolean objects by them selfs.

Let's go through a simple example:

    :obj:`x1+~x2`

The first character is a "x". This indicates a symbol of yet unknown length.
Therefore we search on until we find the end of the symbol and get the
string "x1" which we use to create a new symbol. It's not determined what will
happen with this symbol so we store it in a list of following structure:

    :obj:`[None, None, Symbol("x1")]`

Where the first position contains a pointer to the upper list (in this case
None, since it isn't a subexpression) and the second determines which
operation should be performed with the arguments in the following positions.
Analysing the next character which is a "+" tells us that an OR operation is
performed and we can update the current list to:

    :obj:`[None, OR, Symbol("x1")]`

The next character is a "~" (a NOT operation). Since we don't already know
what will be inside this NOT operation we can't simply append a NOT(?) to
the current list. Instead we will create a new list, storing the old one as
first argument and NOT as second argument:

    :obj:`[[None, OR, Symbol("x1")], NOT]`

Proceeding the parsing we find the symbol "x2" which then is appended to the
current list:

    :obj:`[[None, OR, Symbol("x1")], NOT, Symbol("x2")]`

Afterwards the end of the string is reached and everything can be finished.
First, NOT(Symbol("x2")) is computed and appended to the parent list:

    :obj:`[None, OR, Symbol("x1"), NOT(Symbol("x2"))]`

Then the OR operation can be carried out:

    :obj:`OR(Symbol("x1"), NOT(Symbol("x2")))`



