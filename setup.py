#!/usr/bin/env python

from __future__ import absolute_import

from setuptools import find_packages
from setuptools import setup


long_desc = ''' Boolean Algebra.
This module defines a Boolean Algebra over the set {TRUE, FALSE} with boolean
variables and the boolean functions AND, OR, NOT. For extensive documentation
look either into the docs directory or view it online, at
https://booleanpy.readthedocs.org/en/latest/
https://github.com/bastikr/boolean.py
Copyright (c) 2009-2010 Sebastian Kraemer, basti.kr@gmail.com
Released under revised BSD license.
'''


setup(
    name='boolean.py',
    version='1.0',
    license='revised BSD license',
    description='Boolean Algreba',
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
        'Development Status :: 4 - Beta',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: BSD License',
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
