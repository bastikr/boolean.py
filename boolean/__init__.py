"""
Boolean Algebra.

This module defines a Boolean Algebra over the set {TRUE, FALSE} with boolean
variables and the boolean functions AND, OR, NOT, XOR. For extensive documentation
look either into the docs directory or view it online, at
https://booleanpy.readthedocs.org/en/latest/.

Copyright (c) Sebastian Kraemer, basti.kr@gmail.com and others

SPDX-License-Identifier: BSD-2-Clause
"""

from boolean.boolean import (
    AND,
    NOT,
    OR,
    XOR,
    PARSE_ERRORS,
    TOKEN_AND,
    TOKEN_FALSE,
    TOKEN_LPAR,
    TOKEN_NOT,
    TOKEN_OR,
    TOKEN_RPAR,
    TOKEN_SYMBOL,
    TOKEN_TRUE,
    TOKEN_XOR,
    BooleanAlgebra,
    Expression,
    ParseError,
    Symbol,
)
