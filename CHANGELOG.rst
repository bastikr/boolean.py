
Changelog
=========


next
------------------


3.3.0 (2017-02-09)
------------------

* API changes

 * #40 and #50 Expression.subs() now takes 'default' thanks to @kronuz
 * #45 simplify=False is now the default for parse and related functions or methods.
 * #40 Use "&" and "|" as default operators

* Bug fixes

 * #60 Fix bug for "a or b c" which is not a valid expression
 * #58 Fix math formula display in docs
 * Improve handling of parse errors


2.0.0 (2016-05-11)
------------------

* API changes
 * New algebra definition. Refactored class hierarchy. Improved parsing.


* New features

 * possibility to subclass algebra definition
 * new normal forms shortcuts for DNF and CNF.


1.1 (2016-04-06)
------------------

* Initial release on Pypi.
