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

BaseSet = collections.namedtuple("BaseSet", ("TRUE", "FALSE"))
BaseOperations = collections.namedtuple("BaseOperations", ("NOT", "AND", "OR"))
Algebra = collections.namedtuple("Algebra",
                                 ("baseset", "operations", "symbol"))


class Expression:
    """
    Base class for all boolean classes.
    """
    _args = None
    _iscanonical = False
    _hash = None
    algebra = None

    def __new__(cls, arg, *, eval=True):
        if isinstance(arg, Expression):
            return arg
        if isinstance(arg, str):
            return parse(arg, eval=eval)
        elif arg in (0, False):
            return FALSE
        elif arg in (1, True):
            return TRUE
        raise TypeError("Wrong argument for Expression.")

    @property
    def args(self):
        """
        Return a tuple of all subterms.
        """
        return self._args

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

        This method doesn't try any transformations, so it will actually return
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
        if hasattr(self, "_cls_order") and hasattr(other, "_cls_order"):
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
    _iscanonical = True
    _instance = None
    _str = None
    _repr = None
    _cls_order = 0

    def __new__(cls, arg=None, *, eval=False):
        if arg is not None:
            if isinstance(arg, BaseElement):
                return arg
            elif arg in (0, False):
                return FALSE
            elif arg in (1, True):
                return TRUE
            else:
                raise TypeError("Bad argument: %s" % arg)
        elif cls is BaseElement:
            raise TypeError("BaseElement can't be created without argument.")
        # Make sure only one instance is created.
        if cls._instance is None:
            obj = object.__new__(cls)
            cls._instance = obj # Save this instance.
            return obj
        return cls._instance # Return the already created instance.

    @property
    def dual(self):
        """
        Return the dual Base Element.

        That means TRUE.dual will return FALSE and FALSE.dual will return
        TRUE.
        """
        bs = self.algebra.baseset
        if self is bs.TRUE:
            return bs.FALSE
        elif self is bs.FALSE:
            return bs.TRUE
        else:
            raise AttributeError("Class should be TRUE or FALSE but is %s."\
                                 % self.cls.__name__)

    def __lt__(self, other):
        cmp = Expression.__lt__(self, other)
        if cmp is not NotImplemented:
            return cmp
        if isinstance(other, BaseElement):
            if self is FALSE:
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
# boolean algebra.
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
    _args = None
    _iscanonical = True
    _cls_order = 5

    def __new__(cls, obj=None, *, eval=False):
        return object.__new__(cls)

    def __init__(self, obj=None, *, eval=False):
        if obj is not None:
            self._args = (obj,)

    @property
    def isliteral(self):
        """
        Return True if object is a literal otherwise False.
        """
        return True

    def __lt__(self, other):
        cmp = Expression.__lt__(self, other)
        if cmp is not NotImplemented:
            return cmp
        if isinstance(other, Symbol):
            if self.args is None:
                if other.args is None:
                    return hash(self) < hash(other) # 2 anonymous symbols.
                else:
                    return False # Anonymous-Symbol < Named-Symbol.
            else:
                if other.args is None:
                    return True # Named-Symbol < Anonymous-Symbol.
                else:
                    return self.args[0].__lt__(other.args[0]) # 2 named symbols.
        return NotImplemented

    def __str__(self):
        if self.args is None:
            return "L" + str(hash(self))
        else:
            return str(self.args[0])

    def __repr__(self):
        if self.args:
            arg = repr(self.args[0])
        else:
            arg = hash(self)
        return "%s(%s)" % (self.__class__.__name__, arg)


class Function(Expression):
    """
    Boolean function.

    A boolean function takes n boolean expressions as arguments (n is called
    the order of the function) and maps them to one of the base elements.
    Typical examples for implemented functions are AND and OR.
    """
    order = (2, float("inf"))
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
        elif term.args[0] in self.algebra.baseset:
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
    operator = "*"
    _cls_order = 10
    _identity = True


class OR(DualBase):
    """
    Boolean OR operation.

    The OR operation takes 2 or more arguments and can also be created by
    using "+" between two boolean expressions.
    """
    operator = "+"
    _cls_order = 25
    _identity = False


BASESET = BaseSet(TRUE=TRUE, FALSE=FALSE)
BASEOPERATIONS = BaseOperations(NOT=NOT, AND=AND, OR=OR)
ALGEBRA = Algebra(BASESET, BASEOPERATIONS, Symbol)
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

class NormalForm(Expression):
    """
    Base class for conjunctive and disjunctive normal form.
    """
    iscanonical = True
    _operation = None

    def __new__(cls, arg, *, eval=True):
        if isinstance(arg, NormalForm):
            return arg
        return object.__new__(cls)

    def __init__(self, arg, *, eval=True):
        # If NormalForm.__new__ is called with a NormalForm instance as
        # argument, this instance will simply be returned. But then python will
        # call its __init__ method with the instance as argument, what will
        # result in an error. The following "if" prevents this error by
        # aborting the __init__ method if it has been called before.
        if self._args:
            return
        if isinstance(arg, Expression):
            args = normalize(self.operation, arg)
        elif isinstance(arg, str):
            args = normalize(self.operation, parse(arg))
        elif arg in (0, False):
            args = (FALSE,)
        elif arg in (1, True):
            args = (TRUE,)
        else:
            raise TypeError("Bad argument: %s" % arg)
        Function.__init__(self, *args)

    @property
    def operation(self):
        """
        Return the operation associated with this Normal Form.

        That means a CNF will return the AND class while the DNF will return
        the OR class.
        This is just a convenient shortcut for FullNormalForm.getoperation.
        """
        return self.getoperation()

    @classmethod
    def getoperation(cls):
        """
        Return the operation associated with this Normal Form.

        That means a CNF will return the AND class while the DNF will return
        the OR class.
        """
        return getattr(cls.algebra.operations, cls._operation)

    def __hash__(self):
        """
        Calculate a hash.

        For more information about equality and hashes, look into the
        documentation.
        """
        if self._hash is None:
            args = self.args
            if len(args) == 1:
                return hash(args[0])
            else:
                op = self.operation
                self._hash = hash(op.__name__) ^ hash(frozenset(args))
                return self._hash
        else:
            return self._hash

    def __eq__(self, other):
        """
        Test if other element is structurally the same as itself.

        This method doesn't try any transformations, so it will actually return
        False although terms are mathematically equal. It only uses the fact
        that all operations are commutative and considers different ordering as
        equal. Actually also idempotence is used, so args can appear more often
        in one term than in the other.
        """
        if isinstance(other, self.operation):
            if len(self.args)<2:
                return False
            else:
                return self.operation(*self.args) == other
        elif isinstance(other, self.__class__):
            return Expression.__eq__(self, other)
        elif len(self.args)==1:
            return self.args[0] == other
        else:
            return NotImplemented

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


class CNF(NormalForm):
    """
    Conjunctive normal form with only prime implicants.
    """
    _operation = "AND"
    _cls_order = 15


class DNF(NormalForm):
    """
    Disjunctive normal form with only prime implicants.
    """
    _operation = "OR"
    _cls_order = 30


class FullNormalForm(Expression):
    """
    Base class for full conjunctive and disjunctive normal forms.

    **References**

    1. A. Dusa, "A mathematical approach to the boolean minimization problem",
       http://www.compasss.org/files/wpfiles/Dusa2007.pdf
    """
    _primerows = None
    _primechart = None
    _pow = {}

    def __new__(cls, arg, symbols=None, *, eval=True):
        if isinstance(arg, FullNormalForm):
            return arg
        return object.__new__(cls)

    def __init__(self, arg, symbols=None, *, eval=True):
        # If FullNormalForm.__new__ is called with a FullNormalForm instance as
        # argument, this instance will simply be returned. But then python will
        # call its __init__ method with the instance as argument, what will
        # result in an error. The following "if" prevents this error by
        # aborting the __init__ method if it has been called before.
        # TODO: Improve __init__ method of FullNormalForm.
        if self._args:
            return
        try:
            for symbol in symbols:
                if not isinstance(symbol, Symbol):
                    raise TypeError()
        except TypeError:
            raise TypeError("'symbols' must be a list of Symbols.")
        if isinstance(arg, Expression):
            rows = self._boolexpr2rows(arg, symbols)
        elif isinstance(arg, str):
            rows = self._boolexpr2rows(parse(arg), symbols)
        elif arg in (0, False):
            rows = self._boolexpr2rows(FALSE, symbols)
        elif arg in (1, True):
            rows = self._boolexpr2rows(TRUE, symbols)
        elif hasattr(arg, "__iter__"):
            rows = arg
        self.length = len(symbols)
        self.symbols = symbols
        self.rowsbase3 = tuple(self._rowsbase2_as_rowsbase3(rows, self.length))

    @property
    def operation(self):
        """
        Return the operation associated with this Full Normal Form.

        That means a FCNF will return the AND class while the FDNF will return
        the OR class.
        """
        return self.getoperation()

    @classmethod
    def getoperation(cls):
        """
        Return the operation associated with this Full Normal Form.

        That means a FCNF will return the AND class while the FDNF will return
        the OR class.
        """
        return getattr(cls.algebra.operations, cls._operation)

    @classmethod
    def _boolexpr2rows(cls, expr, symbols):
        if not isinstance(expr, Expression):
            raise TypeError("'expr' must be a boolean expression.")
        if isinstance(expr, TRUE):
            length = len(symbols)
            return tuple(cls._rowsbase2_as_rowsbase2(range(length), length))
        elif isinstance(expr, FALSE):
            return ()
        args = normalize(cls.getoperation(), expr)
        return tuple(cls.term2row(term, symbols) for term in symbols)


    @staticmethod
    def row2digits(row, base, length):
        """
        Return a digital representation of row in the given base.
        """
        digits = [0]*length
        while row != 0:
            length -= 1
            row, digits[length] = divmod(row, base)
        return tuple(digits)

    @staticmethod
    def digits2row(digits, base):
        """
        Transform the digits into their corresponding row.
        """
        length = len(digits)
        if self._pow.haskey(base) and len(self._pow[base]) >= length:
            pow = self._pow[base]
        else:
            pow = tuple(base**(length-1-i) for i in range(length))
            self._pow[base] = pow
        return sum(pow[j]*d for j, d in enumerate(digits))

    @classmethod
    def row2term(cls, row, base, symbols):
        """
        Return the boolean expression corresponding to the given row.
        """
        digits = cls.row2digits(row, base, len(symbols))
        literals = []
        for i, symbol in enumerate(symbols):
            if digits[i] == 1:
                l = NOT(symbol, eval=False)
                l._iscanonical = True
                literals.append(l)
            elif digits[i] == 2:
                literals.append(symbol)
        term = cls.getoperation().getdual()(*literals, eval=False)
        term._iscanonical = True
        return term

    @classmethod
    def term2row(cls, term, symbols):
        """
        Return the row in base corresponding to the boolean expression.
        """
        length = len(symbols)
        digits = [0]*length
        for i, symbol in enumerate(symbols):
            if symbol in term.args:
                digits[i] = 2
            elif NOT(symbol) in term.args:
                digits[i] = 1
        return cls.digits2row(digits, 3, length)

    @classmethod
    def _rowsbase2_as_rowsbase3(cls, rowsbase2, length):
        """
        Transform rows given in base 2, into rows in base 3.
        """
        # TODO: Adrian Dusa describes a method that he claims to be superior.
        # TODO: Use cached powers.
        pow = tuple(3**(length-1-i) for i in range(length))
        rowsbase3 = [None]*len(rowsbase2)
        for i, rowb2 in enumerate(rowsbase2):
            # Calculate binary representation of row, add 1 to every digit and
            # then transform the binary representaion into the corresponding row.
            digits = cls.row2digits(rowb2, 2, length)
            rowsbase3[i] = sum(pow[j]*(d+1) for j, d in enumerate(digits))
        return tuple(rowsbase3)

    @classmethod
    def _forward_terms(cls, row, length):
        """
        Calculate all rows with which the given row can be combined.

        Returns
        -------
        A list of row numbers.
        """
        # TODO: Use cached powers.
        terms = []
        for i, digit in enumerate(cls.row2digits(row, 3, length)):
            if digit == 1:
                terms.append(row+3**(length-1-i))
        return terms

    @classmethod
    def _combine(cls, rows, length):
        """
        Combine all rows with each other combinable row.

        Returns
        -------
        combined_rows
            A list of all rows that could be obtained by combination of
            the given rows.
        unused_rows
            A list of rows that were not used for any combination.
        """
        was_used = [False]*len(rows)
        combined_rows = []
        for i, row in enumerate(rows):
            fterms = cls._forward_terms(row, length)
            for fterm in fterms:
                if fterm in rows:
                    combinedterm = 2*row-fterm
                    if combinedterm not in combined_rows:
                        combined_rows.append(combinedterm)
                    was_used[i] = was_used[rows.index(fterm)] = True
        unused_rows = tuple(rows[i] for i, used in enumerate(was_used)\
                                                if used==False)
        return combined_rows, unused_rows

    @classmethod
    def _expand(cls, row, length):
        """
        Return all primary rows that are combined to the given row.
        """
        digits = cls.row2digits(row, 3, length)
        A = []
        for pos, digit in enumerate(digits):
            if digit == 0:
                a = 3**(length-pos-1)
                A.append((a, 2*a))
        if A:
            return tuple(row+sum(i) for i in itertools.product(*A))
        else:
            return (row,)

    @property
    def primerows(self):
        """
        Calculate the primerows of this FullNormalForm.

        Returns
        -------
        A sorted tuple of primerows.
        """
        if self._primerows is not None:
            return self._primerows
        combined_rows = self.rowsbase3
        unused_rows = []
        while combined_rows:
            combined_rows, unused = self._combine(combined_rows, self.length)
            unused_rows.extend(unused)
        unused_rows.sort()
        self._primerows = tuple(unused_rows)
        return self._primerows

    @property
    def primeimplicants(self):
        """
        Calculate prime implicants of this FullNormalForm.

        Returns
        -------
        A sorted tuple of prime implicants. (Sorted by their corresponding
        row number)
        """
        symbols = self.symbols
        return tuple(self.row2term(row, 3, symbols) for row in self.primerows)

    @property
    def primerowchart(self):
        """
        Calculate the prime implicant chart in row representation.

        Returns
        -------
        A tuple with one entry for each primerow, holding the row number of
        terms that were combined to that specific primerow.
        """
        if self._primechart is not None:
            return self._primechart
        length = self.length
        rows = self.primerows
        self._primechart = tuple(self._expand(row, length) for row in rows)
        return self._primechart

    @property
    def reduced_primerowchart(self):
        """
        Calculate the reduced primechart in row representation.
        """
        # TODO: Write simpler?, faster? implementation of reduced_primechart.
        terms = list(self.rowsbase3)
        primerows = list(self.primerows)
        chart = list(map(frozenset, self.primerowchart))
        def dce():
            """
            Dominating column elimination.
            """
            tchart = [None]*len(terms)
            for i, term in enumerate(terms):
                tchart[i] = frozenset(row for j, row in enumerate(primerows)\
                                          if term in chart[j])
            changed = False
            finished = False
            while not finished:
                finished = True
                for col1, col2 in itertools.combinations(tchart, 2):
                    l1, l2 = len(col1), len(col2)
                    if l1 > l2:
                        col1, col2 = col2, col1
                    if col1.issubset(col2):
                        # Eliminate col2
                        i = tchart.index(col2)
                        del tchart[i], terms[i]
                        changed = True
                        finished = False
                        break
            for i, row in enumerate(primerows):
                chart[i] = frozenset(col for j, col in enumerate(terms)\
                                         if row in tchart[j])
            return changed

        def dre():
            """
            Dominated rows elemination.
            """
            changed = False
            finished = False
            while not finished:
                finished = True
                for row1, row2 in itertools.combinations(chart, 2):
                    l1, l2 = len(row1), len(row2)
                    if l1 == l2:
                        continue
                    elif l1 > l2:
                        row1, row2 = row2, row1
                    if row1.issubset(row2):
                        i = chart.index(row1)
                        del chart[i], primerows[i]
                        changed = True
                        finished = False
                        break
            return changed
        changed = True
        while changed:
            changed = dce() or dre()
        return primerows, chart

    @property
    def minimal_rows(self):
        primerows, chart = self.reduced_primerowchart
        terms = frozenset.union(*chart)
        tchart = [None] * len(terms)
        for i, term in enumerate(terms):
            tchart[i] = frozenset(row for j, row in enumerate(primerows)\
                                      if term in chart[j])
        # TODO: Improve minimal_rows algorithm.
        # It may be better to do simplifactions while multiplying out.
        terms = map(frozenset, itertools.product(*tchart))
        term0 = terms.__next__()
        solutions = set((term0,))
        minlength = len(term0)
        for term in terms:
            length = len(term)
            if length < minlength:
                solutions = set((term,))
                minlength = length
            elif length == minlength:
                solutions.add(term)
        return solutions


class FCNF(FullNormalForm):
    _cls_order = 20
    _operation = "AND"

class FDNF(FullNormalForm):
    _cls_order = 35
    _operation = "OR"


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

