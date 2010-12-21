"""
Boolean Algebra.

This module defines a Boolean Algebra over the set {TRUE, FALSE} with boolean
variables and the boolean functions AND, OR, NOT. For extensive documentation
look either into the docs directory or view it online.

Copyright (c) 2009-2010 Sebastian Kraemer, basti.kr@gmail.com
Released under revised BSD license.
"""
import itertools
import collections

# A boolean algebra is defined by its base elements (=domain), its operations
# (in this case only NOT, AND and OR) and an additional "symbol" type.
Algebra = collections.namedtuple("Algebra",
                                 ("domain", "operations", "symbol"))

# Defines the two base elements TRUE and FALSE for the algebra.
BooleanDomain = collections.namedtuple("BooleanDomain", ("TRUE", "FALSE"))

# Defines the basic boolean operations NOT, AND and OR.
BooleanOperations = collections.namedtuple("BooleanOperations",
                                           ("NOT", "AND", "OR"))


class Expression:
    """
    Base class for all boolean expressions.
    """
    # Used to store subterms. Can be empty.
    _args = None
    # Defines order relation between different classes.
    _cls_order = None
    # Stores if an expression is already canonical.
    _iscanonical = False
    # Cashes the hash value for an expression. (Expressions are immutable)
    _hash = None
    # Stores an object associated to this boolean expression.
    _obj = None

    # Holds an Algebra tuple which defines the boolean algebra.
    algebra = None

    def __new__(cls, arg, *, eval=True):
        if isinstance(arg, Expression):
            return arg
        if isinstance(arg, str):
            return parse(arg, eval=eval)
        elif arg in (0, False):
            return cls.algebra.domain.FALSE
        elif arg in (1, True):
            return cls.algebra.domain.TRUE
        raise TypeError("Wrong argument for Expression.")

    @property
    def args(self):
        """
        Return a tuple of all subterms.
        """
        return self._args

    @property
    def obj(self):
        """
        Return the associated object of this object.

        Might be None.
        """
        return self._obj

    @property
    def objects(self):
        """
        Return a set off all associated objects in this expression.

        Might be an empty set.
        """
        s = set() if self.obj is None else set([self.obj])
        if self.args is not None:
            for arg in self.args:
                s |= arg.objects
        return s

    @property
    def isliteral(self):
        """
        Return True if object is a literal otherwise False.
        """
        return False # This is overriden in all Literals.

    @property
    def literals(self):
        """
        Return a set of all literals contained in this or any subexpression.
        """
        if self.isliteral:
            return set((self,))
        if self.args is None:
            return set()
        else:
            s = set()
            for arg in self.args:
                s |= arg.literals
            return s

    def literalize(self):
        """
        Return an expression where NOTs are only occuring as literals.
        """
        if self.isliteral or self.args is None:
            return self
        args = tuple(arg.literalize() for arg in self.args)
        if all(arg is self.args[i] for i, arg in enumerate(args)):
            return self
        else:
            return self.__class__(*args, eval=False)

    @property
    def symbols(self):
        """
        Return a set of all symbols contained in this or any subexpression.
        """
        if isinstance(self, Symbol):
            return set((self,))
        if self.args is None:
            return set()
        else:
            s = set()
            for arg in self.args:
                s |= arg.symbols
            return s

    def subs(self, subs_dict, *, eval=True):
        """
        Return an expression where all subterms equal to a key are substituted.
        """
        for expr, substitution in subs_dict.items():
            if expr == self:
                return substitution
        expr = self._subs(subs_dict, eval=eval)
        return self if expr is None else expr

    def _subs(self, subs_dict, eval):
        new_args = []
        changed_something = False
        for arg in self.args:
            matched = False
            for expr, substitution in subs_dict.items():
                if arg == expr:
                    new_args.append(substitution)
                    changed_something = matched = True
                    break
            if not matched:
                new_arg = None if arg.args is None else\
                         arg._subs(subs_dict, eval)
                if new_arg is None:
                    new_args.append(arg)
                else:
                    changed_something = True
                    new_args.append(new_arg)

        if changed_something:
            return self.__class__(*new_args, eval=eval)
        else:
            return None

    @property
    def iscanonical(self):
        """
        Return True if the boolean object is in canonical form.
        """
        return self._iscanonical

    def eval(self, **evalkwargs):
        """
        Return a possibly simplified, canonical form of the boolean object.
        """
        return self

    def __hash__(self):
        """
        Calculate a hash respecting the structure of the whole expression.

        This is done by using as first part the classname and as second
        the hash of the arguments stored in a frozenset.
        For more information about equality and hashes, look into the
        documentation.
        # TODO: Add entry about hashes into documentation.
        """
        # The hash consists of two parts, the hash of the class name and the
        # hash of the subterms (stored in args). If the object has no subterms,
        # the id of the object is used instead.
        # Since all boolean objects are immutable the hash only has to be
        # computed once.
        if self._hash is None:
            if self.args is None:
                arghash = id(self)
            else:
                arghash = hash(frozenset(self.args))
            self._hash = hash(self.__class__.__name__) ^ arghash
            return self._hash
        else:
            return self._hash

    def __eq__(self, other):
        """
        Test if other element is structurally the same as itself.

        This method doesn't try any transformations, so it will return
        False although terms are mathematically equal. It only uses the fact
        that all operations are commutative and considers different ordering as
        equal. Actually also idempotence is used, so args can appear more often
        in one term than in the other.
        """
        if self is other:
            return True
        if not isinstance(other, self.__class__):
            return NotImplemented
        if self.args is None or other.args is None:
            return False
        if frozenset(self.args) == frozenset(other.args):
            return True
        return False

    def __ne__(self, other):
        return not self == other

    def __lt__(self, other):
        if self._cls_order is not None and other._cls_order is not None:
            if self._cls_order == other._cls_order:
                return NotImplemented
            else:
                return self._cls_order < other._cls_order
        return NotImplemented

    def __gt__(self, other):
        lt = other.__lt__(self)
        if lt is NotImplemented:
            return not self.__lt__(other)
        else:
            return lt

    def __mul__(self, other):
        return self.algebra.operations.AND(self, other)

    def __invert__(self):
        return self.algebra.operations.NOT(self)

    def __add__(self, other):
        return self.algebra.operations.OR(self, other)


class BaseElement(Expression):
    """
    Base class for the base elements TRUE and FALSE of the boolean algebra.
    """
    _cls_order = 0
    _iscanonical = True

    # The following two attributes define the output of __str__ and __repr__
    # respectively. They are overwritten in the classes TRUE and FALSE.
    _str = None
    _repr = None

    def __new__(cls, arg=None, *, eval=False):
        if arg is not None:
            if isinstance(arg, BaseElement):
                return arg
            elif arg in (0, False):
                return cls.algebra.domain.FALSE
            elif arg in (1, True):
                return cls.algebra.domain.TRUE
            else:
                raise TypeError("Bad argument: %s" % arg)
        elif cls is BaseElement:
            raise TypeError("BaseElement can't be created without argument.")
        if cls.algebra is None:
            return object.__new__(cls)
        elif isinstance(cls.algebra.domain.TRUE, cls):
            return cls.algebra.domain.TRUE
        elif isinstance(cls.algebra.domain.FALSE, cls):
            return cls.algebra.domain.FALSE
        else:
            raise TypeError("BaseElement can only create objects in the\
                             current domain.")

    @property
    def dual(self):
        """
        Return the dual Base Element.

        That means TRUE.dual will return FALSE and FALSE.dual will return
        TRUE.
        """
        domain = self.algebra.domain
        if self is domain.TRUE:
            return domain.FALSE
        elif self is domain.FALSE:
            return domain.TRUE
        else:
            raise AttributeError("Class should be TRUE or FALSE but is %s."\
                                 % self.cls.__name__)

    def __lt__(self, other):
        cmp = Expression.__lt__(self, other)
        if cmp is not NotImplemented:
            return cmp
        if isinstance(other, BaseElement):
            if self is self.algebra.domain.FALSE:
                return True
            return False
        return NotImplemented

    def __str__(self):
        if self._str is not None:
            return self._str

    def __repr__(self):
        if self._repr is not None:
            return self._repr


class _TRUE(BaseElement):
    """
    Boolean base element TRUE.

    This is one of the two elements of the boolean algebra.
    """
    _str = "1"
    _repr = "TRUE"

class _FALSE(BaseElement):
    """
    Boolean base element FALSE.

    This is one of the two elements of the boolean algebra.
    """
    _str = "0"
    _repr = "FALSE"


# Initialize two singletons which will be used as base elements for the
# default boolean algebra.
TRUE = _TRUE()
FALSE = _FALSE()


class Symbol(Expression):
    """
    Boolean variable.

    Symbols (also called boolean variables) can only take on the values TRUE
    or FALSE. They can hold an object that will be used to determine equality.
    These are called "named symbols". Alternatively it's possible to create
    symbols without any argument (or the argument None). That will result in
    "anonymous symbols", which will always be unequal to any other symbol but
    themselfs.
    """
    _cls_order = 5
    _iscanonical = True

    _obj = None

    def __new__(cls, obj=None, *, eval=False):
        return object.__new__(cls)

    def __init__(self, obj=None, *, eval=False):
        self._obj = obj

    @property
    def obj(self):
        """
        Return the object associated with this symbol.
        """
        return self._obj

    @property
    def isliteral(self):
        """
        Return True if object is a literal otherwise False.
        """
        return True

    def __hash__(self):
        """
        Calculate a hash considering eventually associated objects.
        """
        if self._hash is not None:
            return self._hash # Return cached hash.
        else:
            if self.obj is None: # Anonymous symbol.
                myhash = id(self)
            else: # Hash of associated object.
                myhash = hash(self.obj)
            self._hash = myhash
            return myhash

    def __eq__(self, other):
        """
        Test if other element equals to this symbol.
        """
        if self is other:
            return True
        if not isinstance(other, self.__class__):
            return NotImplemented
        if self.obj is None or other.obj is None:
            return False
        else:
            return self.obj == other.obj

    def __lt__(self, other):
        cmp = Expression.__lt__(self, other)
        if cmp is not NotImplemented:
            return cmp
        if isinstance(other, Symbol):
            if self.obj is None:
                if other.obj is None:
                    return hash(self) < hash(other) # 2 anonymous symbols.
                else:
                    return False # Anonymous-Symbol < Named-Symbol.
            else:
                if other.obj is None:
                    return True # Named-Symbol < Anonymous-Symbol.
                else:
                    return self.obj.__lt__(other.obj) # 2 named symbols.
        return NotImplemented

    def __str__(self):
        if self.obj is None:
            return "S<%s>" % str(hash(self))
        else:
            return str(self.obj)

    def __repr__(self):
        if self.obj is not None:
            obj = repr(self.obj)
        else:
            obj = hash(self)
        return "%s(%s)" % (self.__class__.__name__, obj)


class Function(Expression):
    """
    Boolean function.

    A boolean function takes n boolean expressions as arguments (n is called
    the order of the function) and maps them to one of the base elements.
    Typical examples for implemented functions are AND and OR.
    """
    # Specifies how many arguments a function takes. the first number gives a
    # lower limit, the second an upper limit.
    order = (2, float("inf"))

    # Specifies an infix notation of an operator for printing.
    operator = None

    def __new__(cls, *args, eval=True):
        length = len(args)
        order = cls.order
        if eval:
            return cls(*args, eval=False).eval()
        if order[0] > length:
            raise TypeError("Too few arguments. Got %s, but need at least %s."\
                             % (length, order[0]))
        if order[1] < length:
            raise TypeError("Too many arguments. Got %s, but need at most %s."\
                             % (length, order[1]))
        return object.__new__(cls)

    def __init__(self, *args, eval=True):
        # If a function in the __new__ method is evaluated the __init__ method
        # will be called twice. First with the simplified then with original
        # arguments. The following "if" prevents that the simplified ones are
        # overwritten.
        if self._args:
            return
        _args = [None]*len(args)
        # Make sure all arguments are boolean expressions.
        for i, arg in enumerate(args):
            if isinstance(arg, Expression):
                _args[i] = arg
            elif isinstance(arg, str):
                _args[i] = parse(arg)
            elif arg in (0, False):
                _args[i] = FALSE
            elif arg in (1, True):
                _args[i] = TRUE
            else:
                raise TypeError("Bad argument: %s" % arg)
        self._args = tuple(_args)

    def __str__(self):
        args = self.args
        if self.operator is None:
            return "%s(%s)" % (self.__class__.__name__,
                               ", ".join(str(arg) for arg in args))
        elif len(args) == 1:
            if self.isliteral:
                return self.operator + str(args[0])
            else:
                return "%s(%s)" % (self.operator, str(args[0]))
        else:
            args = (str(arg) if arg.isliteral else "(%s)" % arg for arg in args)
            return self.operator.join(args)

    def __repr__(self):
        return "%s(%s)" % (self.__class__.__name__,
                           ", ".join(repr(arg) for arg in self.args))


class NOT(Function):
    """
    Boolean NOT operation.

    The NOT operation takes exactly one argument. If this argument is a Symbol
    the resulting expression is also called a literal.
    The operator "~" can be used as abbrevation for NOT, e.g. instead of
    NOT(x) one can write ~x (where x is some boolean expression). Also for
    printing "~" is used for better readability.
    """
    order = (1, 1)
    operator = "~"

    @property
    def isliteral(self):
        """
        Return True if object is a literal otherwise False.
        """
        if isinstance(self.args[0], Symbol):
            return True
        else:
            return False

    def literalize(self):
        """
        Return an expression where NOTs are only occuring as literals.
        """
        expr = self.demorgan()
        if isinstance(expr, self.__class__):
            return expr
        return expr.literalize()

    def eval(self, **evalkwargs):
        """
        Return a simplified term in canonical form.

        This means double negations are canceled out and all contained boolean
        objects are in their canonical form.
        """
        if self.iscanonical:
            return self
        term = self.cancel()
        if not isinstance(term, self.__class__):
            return term.eval()
        elif term.args[0] in self.algebra.domain:
            return term.args[0].dual
        else:
            expr = self.__class__(term.args[0].eval(**evalkwargs),
                                  eval=False)
            expr._iscanonical = True
            return expr

    def cancel(self):
        """
        Cancel itself and following NOTs as far as possible.

        Returns the simplified expression.
        """
        term = self
        while True:
            arg = term.args[0]
            if not isinstance(arg, self.__class__):
                return term
            term = arg.args[0]
            if not isinstance(term, self.__class__):
                return term

    def demorgan(self):
        """
        Return a term where the NOT function is moved inward.

        This is achieved by canceling double NOTs and using de Morgan laws.
        """
        term = self.cancel()
        if term.isliteral or\
                not isinstance(term.args[0], self.algebra.operations):
            return term
        op = term.args[0]
        return op.dual(*tuple(self.__class__(arg, eval=False).cancel()\
                for arg in op.args), eval=False)

    def __lt__(self, other):
        if self.args[0] == other:
            return False
        return self.args[0].__lt__(other)


class DualBase(Function):
    """
    Base class for AND and OR function.

    This class uses the duality principle to combine similar methods of AND
    and OR. Both operations take 2 or more arguments and can be created using
    "+" for OR and "*" for AND.
    """
    # Specifies the identity element for the specific operation. (TRUE for
    # AND and FALSE for OR).
    _identity = None

    @property
    def identity(self):
        """
        Return the identity element for this function.

        This will be TRUE for the AND operation and FALSE for the OR operation.
        """
        return BaseElement(self._identity)

    @property
    def annihilator(self):
        """
        Return the annihilator element for this function.

        This will be FALSE for the AND operation and TRUE for the OR operation.
        """
        return BaseElement(not self._identity)

    @property
    def dual(self):
        """
        Return the dual class of this function.

        This means OR.getdual() returns AND and AND.getdual() returns OR.
        This is just a convenient shortcut for getdual()
        """
        return self.getdual()

    @classmethod
    def getdual(cls):
        """
        Return the dual class of this function.

        This means OR.getdual() returns AND and AND.getdual() returns OR.
        """
        ops = cls.algebra.operations
        if issubclass(cls, ops.OR):
            return ops.AND
        elif issubclass(cls, ops.AND):
            return ops.OR
        else:
            raise AttributeError("Class must be in algebra.operations.")

    def eval(self, **evalkwargs):
        """
        Return a simplified expression in canonical form.

        For simplification of AND and OR following rules are used
        recursively bottom up:
         - Idempotence
         - Commutivity (output is always sorted)
         - Associativity (output doesn't contain same operations nested)
         - Annihilation
         - Identity
         - Complementation
         - Absorption

        Other boolean objects are also in their canonical form.
        """
        # TODO: Refactor DualBase.eval into different "sub-evals".
        # If self is already canonical do nothing.
        if self.iscanonical:
            return self
        ops = self.algebra.operations
        # Otherwise bring arguments into canonical form.
        args = tuple(arg.eval() for arg in self.args)
        # Create new instance of own class with canonical args. "eval" has to
        # be set False - otherwise infinite recursion!
        # TODO: Only create new class if some args changed.
        term = self.__class__(*args, eval=False)
        # Associativity:
        #     (A * B) * C = A * (B * C) = A * B * C
        #     (A + B) + C = A + (B + C) = A + B + C
        term = term.flatten()
        # Annihilation: A * 0 = 0, A + 1 = 1
        if self.annihilator in term.args:
            return self.annihilator
        # Idempotence: A * A = A, A + A = A
        args = []
        for arg in term.args:
            if arg not in args:
                args.append(arg)
        if len(args) == 1:
            return args[0]
        # Identity: A * 1 = A, A + 0 = A
        if self.identity in args:
            args.remove(self.identity)
            if len(args) == 1:
                return args[0]
        # Complementation: A * ~A = 0, A + ~A = 1
        for arg in args:
            if ops.NOT(arg) in args:
                return self.annihilator
        # Elemination: (A * B) + (A * ~B) = A, (A + B) * (A + ~B) = A
        i = 0
        while i < len(args)-1:
            j = i + 1
            ai = args[i]
            if isinstance(ai, self.dual):
                while j < len(args):
                    aj = args[j]
                    if isinstance(aj, self.dual) and\
                                len(ai.args)==len(aj.args):
                        # Find terms where only one arg is different.
                        negated = None
                        for arg in ai.args:
                            if arg in aj.args:
                                pass
                            elif ops.NOT(arg, eval=False).cancel() in aj.args:
                                if negated is None:
                                    negated = arg
                                else:
                                    negated = None
                                    break
                            else:
                                negated = None
                                break
                        # If the different arg is a negation simplify the term.
                        if negated is not None:
                            # Cancel out one of the two terms.
                            del args[j]
                            aiargs = list(ai.args)
                            aiargs.remove(negated)
                            if len(aiargs) == 1:
                                args[i] = aiargs[0]
                            else:
                                args[i] = self.dual(*aiargs, eval=False)
                            if len(args) == 1:
                                return args[0]
                            else:
                                # Now the other simplifications have to be
                                # redone.
                                return self.__class__(*args, eval=True)
                    j += 1
            i += 1
        # Absorption: A * (A + B) = A, A + (A * B) = A
        # Negative absorption: A * (~A + B) = A * B, A + (~A * B) = A + B
        i = 0
        while i < len(args):
            j = 0 if i != 0 else 1
            ai = args[i]
            isdual = True if isinstance(ai, self.dual) else False
            while j < len(args):
                aj = args[j]
                if isinstance(aj, self.dual):
                    if ai in aj.args or\
                         (isdual and all(arg in aj.args for arg in ai.args)):
                        del args[j]
                    elif isdual:
                        negated = None
                        for arg in ai.args:
                            if arg in aj.args:
                                pass
                            elif ops.NOT(arg, eval=False).cancel() in aj.args:
                                if negated is None:
                                    negated = arg
                                else:
                                    negated = None
                                    break
                            else:
                                negated = None
                                break
                        if negated is not None:
                            ajargs = list(aj.args)
                            ajargs.remove(ops.NOT(negated, eval=False).cancel())
                            args[j] = self.dual(*ajargs, eval=False)\
                                        if len(ajargs) > 1 else ajargs[0]
                            return self.__class__(*args, eval=True)
                        else:
                            j += 1 if i != j+1 else 2
                    elif ops.NOT(ai, eval=False).cancel() in aj.args:
                        ajargs = list(aj.args)
                        ajargs.remove(ops.NOT(ai, eval=False).cancel())
                        args[j] = self.dual(*ajargs, eval=False)\
                                    if len(ajargs) > 1 else ajargs[0]
                        return self.__class__(*args, eval=True)
                    else:
                        j += 1 if i != j+1 else 2
                else:
                    j += 1 if i != j+1 else 2
            i += 1
        if len(args) == 1:
            return args[0]
        # Commutivity: A * B = B * A, A + B = B + A
        args.sort()
        # Create new (now canonical) expression.
        term = self.__class__(*args, eval=False)
        term._iscanonical = True
        return term

    def flatten(self):
        """
        Return a term where nested terms are flattened as far as possible.

        E.g. A * (B * C) becomes A * B * C.
        """
        args = list(self.args)
        i = 0
        for arg in self.args:
            if isinstance(arg, self.__class__):
                args[i:i+1] = arg.args
                i += len(arg.args)
            else:
                i += 1
        return self.__class__(*args, eval=False)

    def distributive(self):
        """
        Return a term where the leading AND or OR terms are switched.

        This is done by applying the distributive laws:
            A * (B+C) = (A*B) + (A*C)
            A + (B*C) = (A+B) * (A+C)
        """
        dual = self.dual
        args = list(self.args)
        for i, arg in enumerate(args):
            if isinstance(arg, dual):
                args[i] = arg.args
            else:
                args[i] = (arg,)
        prod = itertools.product(*args)
        args = tuple(self.__class__(*arg) for arg in prod)
        if len(args) == 1:
            return args[0]
        else:
            return dual(*args, eval=False)

    def __lt__(self, other):
        cmp = Expression.__lt__(self, other)
        if cmp is not NotImplemented:
            return cmp
        if isinstance(other, self.__class__):
            lenself = len(self.args)
            lenother = len(other.args)
            for i in range(min(lenself, lenother)):
                if self.args[i] == other.args[i]:
                    continue
                cmp = self.args[i].__lt__(other.args[i])
                if cmp is not NotImplemented:
                    return cmp
            if lenself != lenother:
                return lenself < lenother
        return NotImplemented


class AND(DualBase):
    """
    Boolean AND operation.

    The AND operation takes 2 or more arguments and can also be created by
    using "*" between two boolean expressions.
    """
    _cls_order = 10
    _identity = True
    operator = "*"


class OR(DualBase):
    """
    Boolean OR operation.

    The OR operation takes 2 or more arguments and can also be created by
    using "+" between two boolean expressions.
    """
    _cls_order = 25
    _identity = False
    operator = "+"


# Create a default algebra.
DOMAIN = BooleanDomain(TRUE=TRUE, FALSE=FALSE)
OPERATIONS = BooleanOperations(NOT=NOT, AND=AND, OR=OR)
ALGEBRA = Algebra(DOMAIN, OPERATIONS, Symbol)
Expression.algebra = ALGEBRA


def normalize(operation, expr):
    """
    Transform a expression into its normal form in the given operation.

    Returns a tuple of arguments that will satisfy the condition
    operation(*args) == expr (here mathematical equality is meant) and
    the operation doesn't occur in any arg. Also NOT is only appearing
    in literals.
    """
    dualoperation = operation.getdual()
    # Move NOT inwards.
    expr = expr.literalize()
    # Simplify as much as possible, otherwise rdistributive may take
    # forever.
    expr = expr.eval()
    # Totally flatten everything.
    def rdistributive(expr):
        if expr.isliteral:
            return expr
        args = tuple(rdistributive(arg).eval() for arg in expr.args)
        if len(args) == 1:
            return args[0]
        expr = expr.__class__(*args)
        if isinstance(expr, dualoperation):
            expr = expr.distributive()
        return expr
    expr = rdistributive(expr)
    # Canonicalize
    expr = expr.eval()
    if isinstance(expr, operation):
        args = expr.args
    else:
        args = (expr,)
    return args


def symbols(*args):
    """
    Returns a Symbol for every argument given.
    """
    Symbol = ALGEBRA.symbol
    return tuple(Symbol(arg) for arg in args)


PRECEDENCE = {
    NOT: 5,
    AND: 10,
    OR: 15,
    "(": 20,
}

def parse(expr, eval=True):
    """
    Returns a boolean expression created from the given string.
    """
    if not isinstance(expr, str):
        raise TypeError("Argument must be string but it is %s." % type(expr))

    def start_operation(ast, operation):
        """
        Returns an ast where all operations of lower precedence are finalized.
        """
        op_prec = PRECEDENCE[operation]
        while True:
            if ast[1] is None: # [None, None, x]
                ast[1] = operation
                return ast
            prec = PRECEDENCE[ast[1]]
            if prec > op_prec: # op=*, [ast, +, x, y] -> [[ast, +, x], *, y]
                ast = [ast, operation, ast.pop(-1)]
                return ast
            if prec == op_prec: # op=*, [ast, *, x] -> [ast, *, x]
                return ast
            if ast[0] is None: # op=+, [None, *, x, y] -> [None, +, x*y]
                return [ast[0], operation, ast[1](*ast[2:], eval=eval)]
            else: # op=+, [[ast, *, x], ~, y] -> [ast, *, x, ~y]
                ast[0].append(ast[1](*ast[2:], eval=eval))
                ast = ast[0]

    expr = expr.replace(" ", "")
    length = len(expr)
    ast = [None, None]
    i = 0
    while i < length:
        char = expr[i]
        if char == "1":
            ast.append(TRUE)
        elif char == "0":
            ast.append(FALSE)
        elif char.isalpha():
            j = 1
            while i+j < length and expr[i+j].isalnum():
                j += 1
            ast.append(Symbol(expr[i:i+j]))
            i += j - 1
        elif char == "(":
            ast = [ast, "("]
        elif char == ")":
            while True:
                if ast[0] is None:
                    raise TypeError("Bad closing bracket at position %s." % i)
                if ast[1] is "(":
                    ast[0].append(ast[2])
                    ast = ast[0]
                    break
                ast[0].append(ast[1](*ast[2:], eval=eval))
                ast = ast[0]
        elif char == "~":
            ast = [ast, NOT]
        elif char == "*":
            ast = start_operation(ast, AND)
        elif char == "+":
            ast = start_operation(ast, OR)
        else:
            raise TypeError("Unknown character %s at position %s." % (char, i))
        i += 1
    while True:
        if ast[0] is None:
            if ast[1] is None:
                assert len(ast)==3
                expr = ast[2]
            else:
                expr = ast[1](*ast[2:], eval=eval)
            break
        else:
            ast[0].append(ast[1](*ast[2:], eval=eval))
            ast = ast[0]
    return expr


class BooleanAlgebra:
    """
    Base class for user defined boolean algebras.
    """
    # Stores for every user object a boolean object.
    bool_expr = None
    bool_base = None

    def __init__(self, *, bool_expr=None, bool_base=None):
        self.bool_expr = Symbol(obj=self) if bool_expr is None else bool_expr
        self.bool_base = BooleanBase if bool_base is None else bool_base

    def __hash__(self):
        if isinstance(self.bool_expr, self.bool_expr.algebra.symbol):
            return id(self)
        else:
            return hash(self.bool_expr) ^ id(self)

    def __eq__(self, other):
        if isinstance(self.bool_expr, self.bool_expr.algebra.symbol):
            return True if self is other else False
        else:
            return self.bool_expr == other.bool_expr

    def __ne__(self, other):
        return not self == other

    def __lt__(self, other):
        if isinstance(self.bool_expr, self.bool_expr.algebra.symbol):
            return id(self) < id(other)
        return self.bool_expr < other.bool_expr

    def __gt__(self, other):
        return self.bool_expr > other.bool_expr

    def __mul__(self, other):
        return self.bool_base(bool_expr = self.bool_expr*other.bool_expr)

    def __invert__(self):
        return self.bool_base(bool_expr = ~self.bool_expr)

    def __add__(self, other):
        return self.bool_base(bool_expr = self.bool_expr+other.bool_expr)

