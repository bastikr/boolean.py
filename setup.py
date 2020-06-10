#!/usr/bin/env python

from __future__ import absolute_import

from setuptools import find_packages
from setuptools import setup

long_desc = '''

This library helps you deal with boolean expressions and algebra with variables
and the boolean functions AND, OR, NOT.

You can parse expressions from strings and simplify and compare expressions.
You can also easily create your custom algreba and mini DSL and create custom
tokenizers to handle custom expressions.

For extensive documentation look either into the docs directory or view it online, at
https://booleanpy.readthedocs.org/en/latest/

https://github.com/bastikr/boolean.py

Copyright (c) 2009-2020 Sebastian Kraemer, basti.kr@gmail.com and others
SPDX-License-Identifier: BSD-2-Clause
'''

setup(
    name='boolean.py',
    version='3.8',
    license='BSD-2-Clause',
    description='Define boolean algebras, create and parse boolean '
        'expressions and create custom boolean DSL.',
    long_description=long_desc,
    author='Sebastian Kraemer',
    author_email='basti.kr@gmail.com',
    url='https://github.com/bastikr/boolean.py',
    packages=find_packages(),
    include_package_data=True,
    zip_safe=False,
    test_loader='unittest:TestLoader',
    test_suite='boolean.test_boolean',
    keywords='boolean expression, boolean algebra, logic, expression parser',
    classifiers=[
        'Development Status :: 5 - Production/Stable',
        'Intended Audience :: Developers',
        'Operating System :: OS Independent',
        'Programming Language :: Python',
        'Programming Language :: Python :: 2.7',
        'Programming Language :: Python :: 3',
        'Topic :: Scientific/Engineering :: Mathematics',
        'Topic :: Software Development :: Compilers',
        'Topic :: Software Development :: Libraries',
        'Topic :: Utilities',
    ],
)
