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



Download and installation
-------------------------

    `pip install boolean.py`


License
-------

Simplified BSD License
