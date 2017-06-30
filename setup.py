#!/usr/bin/env python

from __future__ import absolute_import

from setuptools import find_packages
from setuptools import setup

with open('README.rst') as readme:
    long_description = readme.read()

setup(
    name='boolean.py',
    version='3.4',
    license='Simplified BSD license',
    description='Define boolean algebras, create and parse boolean expressions and create custom boolean DSL.',
    long_description=long_description,
    author='Sebastian Kraemer',
    author_email='basti.kr@gmail.com',
    url='https://github.com/bastikr/boolean.py',
    packages=find_packages(),
    include_package_data=True,
    zip_safe=False,
    setup_requires=['pytest-runner'],
    tests_require=['pytest'],
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
