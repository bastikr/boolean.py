"""
Boolean expressions algebra.

This module defines a Boolean algebra over the set {TRUE, FALSE} with boolean
variables called Symbols and the boolean functions AND, OR, NOT. 

Some basic logic comparison are supported: Two expressions can be compared for
equivalence or containment. Furthermore you can simplify an expression and
obtain its normal form.

You can create expressions in Python using familiar boolean operators or parse
expressions from strings. The parsing`easy to extend with your own tokenizer.
You can also subclass some classes to customize how expressions behave and are
presented.

For extensive documentation look either into the docs directory or view it
online, at https://booleanpy.readthedocs.org/en/latest/.

Copyright (c) 2009-2010 Sebastian Kraemer, basti.kr@gmail.com and others
Released under revised BSD license.
"""

from __future__ import absolute_import
from __future__ import unicode_literals

import itertools

try:
    basestring  # Python 2
except NameError:
    basestring = str  # Python 3


# Token types for standard operators and parens
TOKEN_AND = 1
TOKEN_OR = 2
TOKEN_NOT = 3
TOKEN_LPAR = 4
TOKEN_RPAR = 5
TOKEN_TRUE = 6
TOKEN_FALSE = 7
TOKEN_SYMBOL = 8

TOKEN_TYPES = {
    TOKEN_AND: 'AND',
    TOKEN_OR: 'OR',
    TOKEN_NOT: 'NOT',
    TOKEN_LPAR: '(',
    TOKEN_RPAR: ')',
    TOKEN_TRUE: 'TRUE',
    TOKEN_FALSE: 'FALSE',
    TOKEN_SYMBOL: 'SYMBOL',
}


# parsing errors code and messages
PARSE_UNKNOWN_TOKEN = 1
PARSE_UNBALANCED_CLOSING_PARENS = 2
PARSE_INVALID_EXPRESSION = 3

PARSE_ERRORS = {
    PARSE_UNKNOWN_TOKEN: 'Unknown token',
    PARSE_UNBALANCED_CLOSING_PARENS: 'Bad closing parenthesis',
    PARSE_INVALID_EXPRESSION: 'Invalid expression',
}


class ParseError(Exception):
    """
    Raised when the parser or tokemizer encounters a syntax error. Instances of
    this class have attributes token_type, token_string, position, error_code to
    access the details of the error. str() of the exception instance returns a
    formatted message.
    
    """
    def __init__(self, token_type=None, token_string='', position=-1, error_code=0):
        self.token_type = token_type
        self.token_string = token_string
        self.position = position
        self.error_code = error_code

    def __str__(self, *args, **kwargs):
        ttype = ''
        if self.token_type:
            ttype = TOKEN_TYPES.get(self.token_type) or repr(self.token_type)
            ttype = ' for token type: %r' % (ttype)

        tstr = ''
        if self.token_string:
            tstr = ' with value: %r' % (self.token_string)

        pos = ''
        if self.position > 0:
            pos = ' at position: %d' % (self.position)
        emsg = PARSE_ERRORS.get(self.error_code, 'Unknown parsing error')

        return '{emsg){ttype}{tstr}(pos}.'.format(ttype=ttype, tstr=tstr, pos=pos, emsg=emsg)


class BooleanAlgebra(object):
    """
    An algebra is defined by:
    - the types of its operations and Symbol.
    - the tokenizer used when parsing expressions from strings. 
    
    This class also serves as a base class for all boolean expressions,
    including base elements, functions and variable symbols.
    """

    def __init__(self, TRUE_class=None, FALSE_class=None, Symbol_class=None,
                 NOT_class=None, AND_class=None, OR_class=None):
        """
        The types for TRUE, FALSE, NOT, AND, OR and Symbol define the boolean
        algebra elements, operations and Symbol variable. They default to the
        standard classes if not provided.

        You can customize an algebra by providing alternative subclasses of the
        standard types.
        """
        # TRUE and FALSE base elements are algebra-level "singleton" instances
        self.TRUE = self._wrap_type(TRUE_class or _TRUE)
        self.TRUE = self.TRUE()

        self.FALSE = self._wrap_type(TRUE_class or _FALSE)
        self.FALSE = self.FALSE()

        # they cross-reference each other
        self.TRUE.dual = self.FALSE
        self.FALSE.dual = self.TRUE

        # boolean operation types, defaulting to the standard types
        self.NOT = self._wrap_type(NOT_class or NOT)
        self.AND = self._wrap_type(AND_class or AND)
        self.OR = self._wrap_type(OR_class or OR)

        # class used for Symbols
        self.Symbol = self._wrap_type(Symbol_class or Symbol)

        tf_nao = {'TRUE': self.TRUE, 'FALSE': self.FALSE,
                  'NOT': self.NOT, 'AND': self.AND, 'OR': self.OR,
                  'Symbol': self.Symbol}

        # setup cross references such that all algebra types and objects hold an
        # attribute for every other types and objects, including themselves.
        self._cross_refs(tf_nao)

    def _wrap_type(self, base_class):
        """
        Return a new type wrapping the base class using the base class name as
        wrapped type name. .
        """
        wrapped_type = type(base_class.__name__, (base_class,), {})
        return wrapped_type

    def _cross_refs(self, objects):
        """
        Set every object as attributes of every object in an `objects` mapping
        of (attribute name -> object)
        """
        for obj in objects.values():
            for name, value in objects.items():
                setattr(obj, name, value)

    def definition(self):
        """
        Return a tuple of this algebra defined elements and types as:
        (TRUE, FALSE, NOT, AND, OR, Symbol)
        """
        return self.TRUE, self.FALSE, self.NOT, self.AND, self.OR, self.Symbol

    def symbols(self, *args):
        """
        Return a tuple of symbols building a new Symbol from each argument.
        """
        return tuple(map(self.Symbol, args))

    def parse(self, expr, simplify=False):
        """
        Return a boolean expression parsed from `expr` either a unicode string
        or tokens iterable.

        Optionally simplify the expression if `simplify` is True.

        Raise ParseError on errors.
        
        If `expr` is a string, the standard `tokenizer` is used for tokenization
        and the algebra configured Symbol type is used to create Symbol
        instances from Symbol tokens.
    
        If `expr` is an iterable, it should contain 3-tuples of: (token,
        token_string, position). In this case, the `token` can be a Symbol
        instance or one of the TOKEN_* types.
        See the `tokenize()` method for detailed specification.
        """

        precedence = {self.NOT: 5, self.AND: 10, self.OR: 15, TOKEN_LPAR: 20}

        if isinstance(expr, basestring):
            tokenized = self.tokenize(expr)
        else:
            tokenized = iter(expr)

        ast = [None, None]

        for token, tokstr, position in tokenized:
            if token == TOKEN_SYMBOL:
                ast.append(self.Symbol(tokstr))
            elif isinstance(token, Symbol):
                ast.append(token)

            elif token == TOKEN_TRUE:
                ast.append(self.TRUE)
            elif token == TOKEN_FALSE:
                ast.append(self.FALSE)

            elif token == TOKEN_NOT:
                ast = [ast, self.NOT]
            elif token == TOKEN_AND:
                ast = self._start_operation(ast, self.AND, precedence)
            elif token == TOKEN_OR:
                ast = self._start_operation(ast, self.OR, precedence)

            elif token == TOKEN_LPAR:
                ast = [ast, TOKEN_LPAR]
            elif token == TOKEN_RPAR:
                while True:
                    if ast[0] is None:
                        raise ParseError(token, tokstr, position,
                                         PARSE_UNBALANCED_CLOSING_PARENS)
                    if ast[1] is TOKEN_LPAR:
                        ast[0].append(ast[2])
                        ast = ast[0]
                        break
                    subex = ast[1](*ast[2:])
                    ast[0].append(subex)
                    ast = ast[0]
            else:
                raise ParseError(token, tokstr, position, PARSE_UNKNOWN_TOKEN)

        while True:
            if ast[0] is None:
                if ast[1] is None:

                    if len(ast) != 3:
                        raise ParseError(error_code=PARSE_INVALID_EXPRESSION)
                    parsed = ast[2]
                else:
                    parsed = ast[1](*ast[2:])
                break
            else:
                subex = ast[1](*ast[2:])
                ast[0].append(subex)
                ast = ast[0]

        if simplify:
            return parsed.simplify()
        return parsed

    def _start_operation(self, ast, operation, precedence):
        """
        Returns an AST where all operations of lower precedence are finalized.
        """
        op_prec = precedence[operation]
        while True:
            if ast[1] is None:  # [None, None, x]
                ast[1] = operation
                return ast

            prec = precedence[ast[1]]
            if prec > op_prec:  # op=&, [ast, |, x, y] -> [[ast, |, x], &, y]
                ast = [ast, operation, ast.pop(-1)]
                return ast

            if prec == op_prec:  # op=&, [ast, &, x] -> [ast, &, x]
                return ast

            if ast[0] is None:  # op=|, [None, &, x, y] -> [None, |, x&y]
                subexp = ast[1](*ast[2:])
                return [ast[0], operation, subexp]

            else:  # op=|, [[ast, &, x], ~, y] -> [ast, &, x, ~y]
                ast[0].append(ast[1](*ast[2:]))
                ast = ast[0]

    def tokenize(self, expr):
        """
        Return an iterable of 3-tuple describing each token given an expression
        unicode string.
    
        This 3-tuple contains (token, token string, position):
        - token: either a Symbol instance or one of TOKEN_* token types..
        - token string: the original token unicode string.
        - position: some simple object describing the starting position of the
          original token string in the `expr` string. It can be an int for a
          character offset, or a tuple of starting (row/line, column).
    
        The token position is used only for error reporting and can be None or
        empty.
    
        Raise ParseError on errors. The ParseError.args is a tuple of:
        (token_string, position, error message)
    
        You can use this tokenizer as a base to create specialized tokenizers
        for your custom algebra by subclassing BooleanAlgebra. See also the
        tests for other examples of alternative tokenizers.
    
        This tokenizer has these characteristics:
        - The `expr` string can span multiple lines,
        - Whitespace is not significant. 
        - The returned position is the starting character offset of a token.
    
        - A TOKEN_SYMBOL is returned for valid identifiers which is a string
        without spaces. These are valid identifiers:
            - Python identifiers.
            - a string even if starting with digits
            - digits (except for 0 and 1).
            - dotted names : foo.bar consist of one token.
            - names with colons: foo:bar consist of one token.
            These are not identifiers:
            - quoted strings.
            - any punctuation which is not an operation
    
        - Recognized operators are (in any upper/lower case combinations):
            - for and:  '*', '&', 'and'
            - for or: '+', '|', 'or'
            - for not: '~', '!', 'not'
    
        - Recognized special symbols are (in any upper/lower case combinations):
            - True symbols: 1 and True
            - False symbols: 0, False and None
        """
        if not isinstance(expr, basestring):
            raise TypeError('expr must be string but it is %s.' % type(expr))

        # mapping of lowercase token strings to a token type id for the standard
        # operators, parens and common true or false symbols, as used in the
        # default tokenizer implementation.
        TOKENS = {
            '*': TOKEN_AND, '&': TOKEN_AND, 'and': TOKEN_AND,
            '+': TOKEN_OR, '|': TOKEN_OR, 'or': TOKEN_OR,
            '~': TOKEN_NOT, '!': TOKEN_NOT, 'not': TOKEN_NOT,
            '(': TOKEN_LPAR, ')': TOKEN_RPAR,
            '[': TOKEN_LPAR, ']': TOKEN_RPAR,
            'true': TOKEN_TRUE, '1': TOKEN_TRUE,
            'false': TOKEN_FALSE, '0': TOKEN_FALSE, 'none': TOKEN_FALSE
        }

        length = len(expr)
        position = 0
        while position < length:
            tok = expr[position]

            sym = tok.isalpha() or tok == '_'
            if sym:
                position += 1
                while position < length:
                    char = expr[position]
                    if char.isalnum() or char in ('.', ':', '_'):
                        position += 1
                        tok += char
                    else:
                        break
                position -= 1

            try:
                yield TOKENS[tok.lower()], tok, position
            except KeyError:
                if sym:
                    yield TOKEN_SYMBOL, tok, position
                elif tok not in (' ', '\t', '\r', '\n'):
                    raise ParseError(token_string=tok, position=position,
                                     error_code=PARSE_UNKNOWN_TOKEN)

            position += 1

    # TODO: explain what this means exactly
    def _rdistributive(self, expr, op_example):
        """
        Recursively flatten the `expr` expression for the `op_example`
        AND or OR operation instance exmaple.
        """
        if expr.isliteral:
            return expr

        expr_class = expr.__class__

        args = (self._rdistributive(arg, op_example) for arg in expr.args)
        args = tuple(arg.simplify() for arg in args)
        if len(args) == 1:
            return args[0]

        expr = expr_class(*args)

        dualoperation = op_example.dual
        if isinstance(expr, dualoperation):
            expr = expr.distributive()
        return expr

    def normalize(self, expr, operation):
        """
        Return a normalized expression transformed to its normal form in the
        given AND or OR operation.
    
        The new expression arguments will satisfy these conditions:
        - operation(*args) == expr (here mathematical equality is meant)
        - the operation does not occur in any of its arg. 
        - NOT is only appearing in literals (aka. Negation normal form).
        
        The operation must be an AND or OR operation or a subclass.
        """
        # ensure that the operation is not NOT
        assert operation in (self.AND, self.OR,)
        # Move NOT inwards.
        expr = expr.literalize()
        # Simplify first otherwise _rdistributive() may take forever.
        expr = expr.simplify()
        operation_example = operation(self.TRUE, self.FALSE)
        expr = self._rdistributive(expr, operation_example)
        # Canonicalize
        expr = expr.simplify()
        if isinstance(expr, operation):
            return expr
        return operation(*expr.args)

    def cnf(self, expr):
        """
        Return a conjunctive normal form of the `expr` expression.
        """
        return self.normalize(expr, self.AND)

    def dnf(self, expr):
        """
        Return a disjunctive normal form of the `expr` expression.
        """
        return self.normalize(expr, self.OR)


class Expression(object):
    """
    Abstract base class for all boolean expressions, including functions and
    variable symbols.
    """
    # Defines sort and comparison order between expressions arguments
    sort_order = None

    # Store arguments aka. subterms of this expressions.
    # subterms are either literals or expressions.
    args = tuple()

    # True is this is a literal expression such as a Symbol, TRUE or FALSE
    isliteral = False

    # True if this expression has been simplified to in canonical form.
    iscanonical = False

    # these class attributes are configured when a new BooleanAlgebra is created
    TRUE = None
    FALSE = None
    NOT = None
    AND = None
    OR = None
    Symbol = None

    @property
    def objects(self):
        """
        Return a set of all associated objects with this expression symbols.
        Include recursively subexpressions objects.
        """
        return set(s.obj for s in self.symbols)

    def get_literals(self):
        """
        Return a list of all the literals contained in this expression.
        Include recursively subexpressions symbols.
        This includes duplicates.
        """
        if self.isliteral:
            return [self]
        if not self.args:
            return []
        return list(itertools.chain.from_iterable(arg.literals for arg in self.args))

    @property
    def literals(self):
        """
        Return a set of all literals contained in this expression.
        Include recursively subexpressions literals.
        """
        return set(self.get_literals())

    def literalize(self):
        """
        Return an expression where NOTs are only occurring as literals.
        Applied recursively to subexpressions.
        """
        if self.isliteral:
            return self
        args = tuple(arg.literalize() for arg in self.args)
        if all(arg is self.args[i] for i, arg in enumerate(args)):
            return self

        return self.__class__(*args)

    def get_symbols(self):
        """
        Return a list of all the symbols contained in this expression.
        Include recursively subexpressions symbols.
        This includes duplicates.
        """
        return [s for s in self.literals if isinstance(s, Symbol)]

    @property
    def symbols(self,):
        """
        Return a list of all the symbols contained in this expression.
        Include recursively subexpressions symbols.
        This includes duplicates.
        """
        return set(self.get_symbols())

    def subs(self, substitutions, simplify=False):
        """
        Return an expression where the expression or all subterms equal to a key
        expression are substituted with the corresponding value expression using
        a mapping of: {expr->expr to substitute.}

        Return this expression unmodified if nothing could be substituted.

        Note that this can be used to tested for expression containment.
        """
        for expr, substitution in substitutions.items():
            if expr == self:
                return substitution

        expr = self._subs(substitutions, simplify=simplify)
        return self if expr is None else expr

    def _subs(self, substitutions, simplify=False):
        """
        Return an expression where all subterms equal to a key expression are
        substituted by the corresponding value expression using a mapping of:
        {expr->expr to substitute.}
        """
        new_args = []
        changed_something = False
        for arg in self.args:
            matched = False
            for expr, substitution in substitutions.items():
                if arg == expr:
                    new_args.append(substitution)
                    changed_something = matched = True
                    break

            if not matched:
                # FIXME: this is not right
                new_arg = None if not arg.args else arg._subs(substitutions, simplify)
                if new_arg is None:
                    new_args.append(arg)
                else:
                    changed_something = True
                    new_args.append(new_arg)

        if changed_something:
            newexpr = self.__class__(*new_args)
            if simplify:
                newexpr = newexpr.simplify()
            return newexpr

    def simplify(self):
        """
        Return a new simplified expression in canonical form built from this
        expression. The simplified expression may be exactly the same as this
        expression.

        Subclasses override this method to compute actual simplification.
        """
        return self

    def __hash__(self):
        """
        Expressions are immutable and hashable. The hash of Functions is
        computed by respecting the structure of the whole expression by mixing
        the class name hash and the recursive hash of a frozenset of arguments.
        Hash of elements is based on their boolean equivalent. Hash of symbols
        is based on their object.
        """
        if not self.args:
            arghash = id(self)
        else:
            arghash = hash(frozenset(map(hash, self.args)))
        return hash(self.__class__.__name__) ^ arghash

    def __eq__(self, other):
        """
        Test if other element is structurally the same as itself.

        This method does not make any simplification or transformation, so it
        will return False although the expression terms may be mathematically
        equal. Use simplify() before testing equality.
        
        For literals, plain equality is used. 
        For functions, it uses the facts that operations are:
        - commutative and considers different ordering as equal.
        - idempotent, so args can appear more often in one term than in the other.
        """
        if self is other:
            return True

        if isinstance(other, self.__class__):
            return frozenset(self.args) == frozenset(other.args)

        return NotImplemented

    def __ne__(self, other):
        return not self == other

    def __lt__(self, other):
        if self.sort_order is not None and other.sort_order is not None:
            if self.sort_order == other.sort_order:
                return NotImplemented
            return self.sort_order < other.sort_order
        return NotImplemented

    def __gt__(self, other):
        lt = other.__lt__(self)
        if lt is NotImplemented:
            return not self.__lt__(other)
        return lt

    def __and__(self, other):
        return self.AND(self, other)

    __mul__ = __and__

    def __invert__(self):
        return self.NOT(self)

    def __or__(self, other):
        return self.OR(self, other)

    __add__ = __or__

    def __bool__(self):
        raise TypeError('Cannot evaluate expression as a Python Boolean.')

    __nonzero__ = __bool__


class BaseElement(Expression):
    """
    Abstract base class for the base elements TRUE and FALSE of the boolean
    algebra.
    """
    sort_order = 0

    def __init__(self):
        super(BaseElement, self).__init__()
        self.iscanonical = True

        # The dual Base Element class for this element: TRUE.dual returns
        # _FALSE() and FALSE.dual returns _TRUE(). This is a cyclic reference
        # and therefore only assigned after creation of the singletons,
        self.dual = None

    def __lt__(self, other):
        if isinstance(other, BaseElement):
            return self == self.FALSE
        return NotImplemented

    __nonzero__ = __bool__ = lambda s: None

    def pretty(self, indent=0, debug=False):
        """
        Return a pretty formatted representation of self.
        """
        return (' ' * indent) + repr(self)


class _TRUE(BaseElement):
    """
    Boolean base element TRUE.
    Not meant to be subclassed nor instantiated directly.
    """

    def __init__(self):
        super(_TRUE, self).__init__()
        # assigned at singleton creation: self.dual = FALSE

    def __hash__(self):
        return hash(True)

    def __eq__(self, other):
        return self is other or other is True or isinstance(other, _TRUE)

    def __str__(self):
        return '1'

    def __repr__(self):
        return 'TRUE'

    __nonzero__ = __bool__ = lambda s: True


class _FALSE(BaseElement):
    """
    Boolean base element FALSE.
    Not meant to be subclassed nor instantiated directly.
    """

    def __init__(self):
        super(_FALSE, self).__init__()
        # assigned at singleton creation: self.dual = TRUE

    def __hash__(self):
        return hash(False)

    def __eq__(self, other):
        return self is other or other is False or isinstance(other, _FALSE)

    def __str__(self):
        return '0'

    def __repr__(self):
        return 'FALSE'

    __nonzero__ = __bool__ = lambda s: False


class Symbol(Expression):
    """
    Boolean variable.
    
    A Symbol can hold an object used to determine equality between symbols.
    """

    # FIXME: the statement below in the original docstring is weird: Symbols do
    # not have a value assigned, so how could they ever be FALSE
    """
    Symbols (also called boolean variables) can only take on the values TRUE or
    FALSE. 
    """

    sort_order = 5

    def __init__(self, obj):
        super(Symbol, self).__init__()
        # Store an associated object. This object determines equality
        self.obj = obj
        self.iscanonical = True
        self.isliteral = True

    def __hash__(self):
        if self.obj is None:  # Anonymous Symbol.
            return id(self)
        return hash(self.obj)

    def __eq__(self, other):
        if self is other:
            return True
        if isinstance(other, self.__class__):
            return self.obj == other.obj
        return NotImplemented

    def __lt__(self, other):
        comparator = Expression.__lt__(self, other)
        if comparator is not NotImplemented:
            return comparator
        if isinstance(other, Symbol):
            return self.obj < other.obj
        return NotImplemented

    def __str__(self):
        return str(self.obj)

    def __repr__(self):
        obj = "'%s'" % self.obj if isinstance(self.obj, basestring) else repr(self.obj)
        return '%s(%s)' % (self.__class__.__name__, obj)

    def pretty(self, indent=0, debug=False):
        """
        Return a pretty formatted representation of self.
        """
        debug_details = ''
        if debug:
            debug_details += '<isliteral=%r, iscanonical=%r>' % (self.isliteral, self.iscanonical)

        obj = "'%s'" % self.obj if isinstance(self.obj, basestring) else repr(self.obj)
        return (' ' * indent) + ('%s(%s%s)' % (self.__class__.__name__, debug_details, obj))


class Function(Expression):
    """
    Boolean function.

    A boolean function takes n (one or more) boolean expressions as arguments
    where n is called the order of the function and maps them to one of the base
    elements TRUE or FALSE. Implemented functions are AND, OR and NOT.
    """

    def __init__(self, *args):
        super(Function, self).__init__()

        # Specifies an infix notation of an operator for printing such as | or &.
        self.operator = None

        assert (all(isinstance(arg, Expression) for arg in args), 
                'Bad arguments: all arguments must be an Expression: %r' % (args,))
        self.args = tuple(args)

    def __str__(self):
        args = self.args
        if len(args) == 1:
            if self.isliteral:
                return '%s%s' % (self.operator, args[0])
            return '%s(%s)' % (self.operator, args[0])

        args_str = []
        for arg in args:
            if arg.isliteral:
                args_str.append(str(arg))
            else:
                args_str.append('(%s)' % arg)

        return self.operator.join(args_str)

    def __repr__(self):
        return '%s(%s)' % (self.__class__.__name__, ', '.join(map(repr, self.args)))

    def pretty(self, indent=0, debug=False):
        """
        Return a pretty formatted representation of self as an indented tree.

        If debug is True, also prints debug information for each expression arg.

        For example:
        >>> print Expression().parse(u'not a and not b and not (a and ba and c) and c or c').pretty()
        OR(
          AND(
            NOT(Symbol('a')),
            NOT(Symbol('b')),
            NOT(
              AND(
                Symbol('a'),
                Symbol('ba'),
                Symbol('c')
              )
            ),
            Symbol('c')
          ),
          Symbol('c')
        )
        """
        debug_details = ''
        if debug:
            debug_details += '<isliteral=%r, iscanonical=%r' % (self.isliteral, self.iscanonical)
            identity = getattr(self, 'identity', None)
            if identity is not None:
                debug_details += ', identity=%r' % (identity)

            annihilator = getattr(self, 'annihilator', None)
            if annihilator is not None:
                debug_details += ', annihilator=%r' % (annihilator)

            dual = getattr(self, 'dual', None)
            if dual is not None:
                debug_details += ', dual=%r' % (dual)
            debug_details += '>'
        cls = self.__class__.__name__
        args = [a.pretty(indent=indent + 2, debug=debug) for a in self.args]
        pfargs = ',\n'.join(args)
        cur_indent = ' ' * indent
        new_line = '' if self.isliteral else '\n'
        return '{cur_indent}{cls}({debug_details}{new_line}{pfargs}\n{cur_indent})'.format(**locals())


class NOT(Function):
    """
    Boolean NOT operation.

    The NOT operation takes exactly one argument. If this argument is a Symbol
    the resulting expression is also called a literal.

    The operator "~" can be used as abbreviation for NOT, e.g. instead of NOT(x)
    one can write ~x (where x is some boolean expression). Also for printing "~"
    is used for better readability.
    
    You can subclass to define alternative string representation.
    For example::
    >>> class NOT2(NOT):
        def __init__(self, *args):
            super(NOT2, self).__init__(*args)
            self.operator = '!'
    """

    def __init__(self, arg1):
        super(NOT, self).__init__(arg1)
        self.isliteral = self.args[0].isliteral
        self.operator = '~'

    def literalize(self):
        """
        Return an expression where NOTs are only occurring as literals.
        """
        expr = self.demorgan()
        if isinstance(expr, self.__class__):
            return expr
        return expr.literalize()

    def simplify(self):
        """
        Return a simplified expr in canonical form.

        This means double negations are canceled out and all contained boolean
        objects are in their canonical form.
        """
        if self.iscanonical:
            return self

        expr = self.cancel()
        if not isinstance(expr, self.__class__):
            return expr.simplify()

        if expr.args[0] in (self.TRUE, self.FALSE,):
            return expr.args[0].dual

        expr = self.__class__(expr.args[0].simplify())
        expr.iscanonical = True
        return expr

    def cancel(self):
        """
        Cancel itself and following NOTs as far as possible.
        Returns the simplified expression.
        """
        expr = self
        while True:
            arg = expr.args[0]
            if not isinstance(arg, self.__class__):
                return expr
            expr = arg.args[0]
            if not isinstance(expr, self.__class__):
                return expr

    def demorgan(self):
        """
        Return a expr where the NOT function is moved inward.
        This is achieved by canceling double NOTs and using De Morgan laws.
        """
        expr = self.cancel()
        if expr.isliteral or not isinstance(expr.args[0], (self.NOT, self.AND, self.OR)):
            return expr
        op = expr.args[0]
        return op.dual(*(self.__class__(arg).cancel() for arg in op.args))

    def __lt__(self, other):
        return self.args[0] < other

    def pretty(self, indent=1, debug=False):
        """
        Return a pretty formatted representation of self.
        Include additional debug details if `debug` is True.
        """
        debug_details = ''
        if debug:
            debug_details += '<isliteral=%r, iscanonical=%r>' % (self.isliteral, self.iscanonical)
        if self.isliteral:
            pretty_literal = self.args[0].pretty(indent=0, debug=debug)
            return (' ' * indent) + '%s(%s%s)' % (self.__class__.__name__, debug_details, pretty_literal)
        else:
            return super(NOT, self).pretty(indent=indent, debug=debug)


class DualBase(Function):
    """
    Base class for AND and OR function.

    This class uses the duality principle to combine similar methods of AND
    and OR. Both operations take 2 or more arguments and can be created using
    "|" for OR and "&" for AND.
    """

    def __init__(self, arg1, arg2, *args):
        super(DualBase, self).__init__(arg1, arg2, *args)

        # identity element for the specific operation.
        # This will be TRUE for the AND operation and FALSE for the OR operation.
        self.identity = None

        # annihilator element for this function.
        # This will be FALSE for the AND operation and TRUE for the OR operation.
        self.annihilator = None

        # dual class of this function.
        # This means OR.dual returns AND and AND.dual returns OR.
        self.dual = None

    def __contains__(self, expr):
        """
        Test if expr is a subterm of this expression.
        """
        if expr in self.args:
            return True

        if isinstance(expr, self.__class__):
            return all(arg in self.args for arg in expr.args)

    def simplify(self):
        """
        Return a new simplified expression in canonical form from this
        expression.

        For simplification of AND and OR fthe ollowing rules are used
        recursively bottom up:
         - Associativity (output does not contain same operations nested)
         - Annihilation
         - Idempotence
         - Identity
         - Complementation
         - Elimination
         - Absorption
         - Commutativity (output is always sorted)

        Other boolean objects are also in their canonical form.
        """
        # TODO: Refactor DualBase.simplify into different "sub-evals".

        # If self is already canonical do nothing.
        if self.iscanonical:
            return self

        # Otherwise bring arguments into canonical form.
        args = [arg.simplify() for arg in self.args]

        # Create new instance of own class with canonical args.
        # TODO: Only create new class if some args changed.
        expr = self.__class__(*args)

        # Literalize before doing anything, this also applies De Morgan's Law
        expr = expr.literalize()

        # Associativity:
        #     (A & B) & C = A & (B & C) = A & B & C
        #     (A | B) | C = A | (B | C) = A | B | C
        expr = expr.flatten()

        # Annihilation: A & 0 = 0, A | 1 = 1
        if self.annihilator in expr.args:
            return self.annihilator

        # Idempotence: A & A = A, A | A = A
        # this boils down to removing duplicates
        args = []
        for arg in expr.args:
            if arg not in args:
                args.append(arg)
        if len(args) == 1:
            return args[0]

        # Identity: A & 1 = A, A | 0 = A
        if self.identity in args:
            args.remove(self.identity)
            if len(args) == 1:
                return args[0]

        # Complementation: A & ~A = 0, A | ~A = 1
        for arg in args:
            if self.NOT(arg) in args:
                return self.annihilator

        # Elimination: (A & B) | (A & ~B) = A, (A | B) & (A | ~B) = A
        i = 0
        while i < len(args) - 1:
            j = i + 1
            ai = args[i]
            if not isinstance(ai, self.dual):
                i += 1
                continue
            while j < len(args):
                aj = args[j]
                if not isinstance(aj, self.dual) or len(ai.args) != len(aj.args):
                    j += 1
                    continue

                # Find terms where only one arg is different.
                negated = None
                for arg in ai.args:
                    # FIXME: what does this pass Do?
                    if arg in aj.args:
                        pass
                    elif self.NOT(arg).cancel() in aj.args:
                        if negated is None:
                            negated = arg
                        else:
                            negated = None
                            break
                    else:
                        negated = None
                        break

                # If the different arg is a negation simplify the expr.
                if negated is not None:
                    # Cancel out one of the two terms.
                    del args[j]
                    aiargs = list(ai.args)
                    aiargs.remove(negated)
                    if len(aiargs) == 1:
                        args[i] = aiargs[0]
                    else:
                        args[i] = self.dual(*aiargs)

                    if len(args) == 1:
                        return args[0]
                    else:
                        # Now the other simplifications have to be redone.
                        return self.__class__(*args).simplify()
                j += 1
            i += 1

        # Absorption: A & (A | B) = A, A | (A & B) = A
        # Negative absorption: A & (~A | B) = A & B, A | (~A & B) = A | B
        args = self.absorb(args)
        if len(args) == 1:
            return args[0]

        # Commutativity: A & B = B & A, A | B = B | A
        args.sort()

        # Create new (now canonical) expression.
        expr = self.__class__(*args)
        expr.iscanonical = True
        return expr

    def flatten(self):
        """
        Return a new expression where nested terms of this expression are
        flattened as far as possible.

        E.g. A & (B & C) becomes A & B & C.
        """
        args = list(self.args)
        i = 0
        for arg in self.args:
            if isinstance(arg, self.__class__):
                args[i:i + 1] = arg.args
                i += len(arg.args)
            else:
                i += 1

        return self.__class__(*args)

    def absorb(self, args):
        """
        Given an `args` sequence of expressions, return a new list of expression
        applying absorption and negative absorption.
        
        See https://en.wikipedia.org/wiki/Absorption_law

        Absorption: A & (A | B) = A, A | (A & B) = A
        Negative absorption: A & (~A | B) = A & B, A | (~A & B) = A | B
        """
        args = list(args)
        if not args:
            args = list(self.args)
        i = 0
        while i < len(args):
            absorber = args[i]
            j = 0
            while j < len(args):
                if j == i:
                    j += 1
                    continue
                target = args[j]
                if not isinstance(target, self.dual):
                    j += 1
                    continue

                # Absorption
                if absorber in target:
                    del args[j]
                    if j < i:
                        i -= 1
                    continue

                # Negative absorption
                neg_absorber = self.NOT(absorber).cancel()
                if neg_absorber in target:
                    b = target.subtract(neg_absorber, simplify=False)
                    if b is None:
                        del args[j]
                        if j < i:
                            i -= 1
                        continue
                    else:
                        args[j] = b
                        j += 1
                        continue

                if isinstance(absorber, self.dual):
                    remove = None
                    for arg in absorber.args:
                        narg = self.NOT(arg).cancel()
                        if arg in target.args:
                            pass
                        elif narg in target.args:
                            if remove is None:
                                remove = narg
                            else:
                                remove = None
                                break
                        else:
                            remove = None
                            break
                    if remove is not None:
                        args[j] = target.subtract(remove, simplify=True)
                j += 1
            i += 1

        return args

    def subtract(self, expr, simplify):
        """
        Return a new expression where the `expr` expression has been removed
        from this expression if it exists.
        """
        args = self.args
        if expr in self.args:
            args = list(self.args)
            args.remove(expr)
        elif isinstance(expr, self.__class__):
            if all(arg in self.args for arg in expr.args):
                args = tuple(arg for arg in self.args if arg not in expr)
        if len(args) == 0:
            return None
        if len(args) == 1:
            return args[0]

        newexpr = self.__class__(*args)
        if simplify:
            newexpr = newexpr.simplify()
        return newexpr

    def distributive(self):
        """
        Return a term where the leading AND or OR terms are switched.

        This is done by applying the distributive laws:
            A & (B|C) = (A&B) | (A&C)
            A | (B&C) = (A|B) & (A|C)
        """
        dual = self.dual
        args = list(self.args)
        for i, arg in enumerate(args):
            if isinstance(arg, dual):
                args[i] = arg.args
            else:
                args[i] = (arg,)

        prod = itertools.product(*args)
        args = tuple(self.__class__(*arg).simplify() for arg in prod)

        if len(args) == 1:
            return args[0]
        else:
            return dual(*args)

    def __lt__(self, other):
        comparator = Expression.__lt__(self, other)
        if comparator is not NotImplemented:
            return comparator

        if isinstance(other, self.__class__):
            lenself = len(self.args)
            lenother = len(other.args)
            for i in range(min(lenself, lenother)):
                if self.args[i] == other.args[i]:
                    continue

                comparator = self.args[i] < other.args[i]
                if comparator is not NotImplemented:
                    return comparator

            if lenself != lenother:
                return lenself < lenother
        return NotImplemented


class AND(DualBase):
    """
    Boolean AND operation, taking 2 or more arguments. 
    
    It can also be created by using "&" between two boolean expressions.

    You can subclass to define alternative string representation.
    For example::
    >>> class AND2(AND):
        def __init__(self, *args):
            super(AND2, self).__init__(*args)
            self.operator = 'AND'
    """

    sort_order = 10

    def __init__(self, arg1, arg2, *args):
        super(AND, self).__init__(arg1, arg2, *args)
        self.identity = self.TRUE
        self.annihilator = self.FALSE
        self.dual = self.OR
        self.operator = '&'


class OR(DualBase):
    """
    Boolean OR operation, taking 2 or more arguments
    
    It can also be created by using "|" between two boolean expressions.

    You can subclass to define alternative string representation.
    For example::
    >>> class OR2(OR):
        def __init__(self, *args):
            super(OR2, self).__init__(*args)
            self.operator = 'OR'
    """

    sort_order = 25

    def __init__(self, arg1, arg2, *args):
        super(OR, self).__init__(arg1, arg2, *args)
        self.identity = self.FALSE
        self.annihilator = self.TRUE
        self.dual = self.AND
        self.operator = '|'
