"""
Boolean Algebra.

This module defines a Boolean Algebra over the set {TRUE, FALSE} with boolean
variables and the boolean functions AND, OR, NOT. For extensive documentation
look either into the docs directory or view it online, at
https://booleanpy.readthedocs.org/en/latest/.

Copyright (c) 2009-2010 Sebastian Kraemer, basti.kr@gmail.com
Released under revised BSD license.
"""

from __future__ import absolute_import

from boolean.boolean import BooleanAlgebra
from boolean.boolean import Symbol

from boolean.boolean import AND
from boolean.boolean import NOT
from boolean.boolean import OR

from boolean.boolean import TOKEN_TRUE
from boolean.boolean import TOKEN_FALSE
from boolean.boolean import TOKEN_SYMBOL

from boolean.boolean import TOKEN_AND
from boolean.boolean import TOKEN_OR
from boolean.boolean import TOKEN_NOT

from boolean.boolean import TOKEN_LPAR
from boolean.boolean import TOKEN_RPAR
