==========
boolean.py
==========

.. image:: https://img.shields.io/travis/bastikr/boolean.py.svg
    :target: https://travis-ci.org/bastikr/boolean.py
.. image:: https://img.shields.io/pypi/wheel/boolean.py.svg
    :target: https://pypi.python.org/pypi/boolean.py/
.. image:: https://img.shields.io/pypi/v/boolean.py.svg
    :target: https://pypi.python.org/pypi/boolean.py/
.. image:: https://img.shields.io/pypi/pyversions/boolean.py.svg
    :target: https://pypi.python.org/pypi/boolean.py/
.. image:: https://img.shields.io/badge/license-BSD-blue.svg
    :target: https://raw.githubusercontent.com/bastikr/boolean.py/master/LICENSE.txt

This python package implements `Boolean algebra`_. It defines two base elements,
:code:`TRUE` and :code:`FALSE`, and a :code:`Symbol` class. Expressions are
built in terms of :code:`AND`, :code:`OR` and :code:`NOT`. Other functions, like
:code:`XOR` and :code:`NAND`, are not implemented but can be emulated with
:code:`AND` or and :code:`NOT`. Expressions are constructed from parsed strings
or in Python.

.. _`Boolean algebra`: https://en.wikipedia.org/wiki/Boolean_algebra

Example
=======

.. code-block:: python

    >>> import boolean
    >>> algebra = boolean.BooleanAlgebra()
    >>> expression1 = algebra.parse(u'apple and (oranges or banana) and not banana', simplify=False)
    >>> expression1
    AND(Symbol('apple'), OR(Symbol('oranges'), Symbol('banana')), NOT(Symbol('banana')))

    >>> expression2 = algebra.parse(u'(oranges | banana) and not banana & apple', simplify=True)
    >>> expression2
    AND(Symbol('apple'), NOT(Symbol('banana')), Symbol('oranges'))

    >>> expression1 == expression2
    False
    >>> expression1.simplify() == expression2
    True

Documentation
=============

http://readthedocs.org/docs/booleanpy/en/latest/

Installation
============

.. code-block:: shell

    pip install boolean.py

Testing
=======

Test :code:`boolean.py` with your current Python environment:

.. code-block:: shell

    python setup.py test

Test with all of the supported Python environments using :code:`tox`:

.. code-block:: shell

    pip install -r test-requirements.txt
    tox

If :code:`tox` throws :code:`InterpreterNotFound`, limit it to python
interpreters that are actually installed on your machine:

.. code-block:: shell

    tox -e py27,py36

License
=======

Simplified BSD License
