boolean.py
==========

"boolean.py" is a small library implementing a boolean algebra. It
defines two base elements, TRUE and FALSE, and a Symbol class that can take
on one of these two values. Calculations are done in terms of AND, OR and
NOT - other compositions like XOR and NAND are not implemented but can be
emulated with AND or and NOT. 
Expressions are constructed from parsed strings or in Python.

It runs on Python 2.7 and Python 3.

https://github.com/bastikr/boolean.py

Build status: [![Build Status](https://travis-ci.org/bastikr/boolean.py.svg?branch=master)](https://travis-ci.org/bastikr/boolean.py)

Example
-------
```
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
```

Documentation
-------------

http://readthedocs.org/docs/booleanpy/en/latest/

Installation
------------

### Installation via pip

To install boolean.py, you need to have the following pieces of software on your computer:

* Python 3.6+
* pip

You then only need to run the following command:

`pip install boolean.py`

### Installation via package managers

There are packages available for easy install on some operating systems. You are welcome to help us package this tool for more distributions!

* Arch Linux (AUR): [python-boolean.py](https://aur.archlinux.org/packages/python-boolean.py/)

Testing
-------

Test `boolean.py` with your current Python environment:

`python setup.py test`

Test with all of the supported Python environments using `tox`:

```
pip install -r test-requirements.txt
tox
```

If `tox` throws `InterpreterNotFound`, limit it to python interpreters that are actually installed on your machine:

```
tox -e py27,py36
```

License
-------

Simplified BSD License
