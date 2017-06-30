import tokenize

try:
    # Python 2
    basestring
except NameError:
    # Python 3
    basestring = str

try:
    from io import StringIO
except ImportError:
    try:
        from cStringIO import StringIO
    except ImportError:
        from StringIO import StringIO

from boolean import BooleanAlgebra, Symbol
from boolean import TOKEN_LPAR, TOKEN_RPAR
from boolean import TOKEN_TRUE, TOKEN_FALSE
from boolean import TOKEN_AND, TOKEN_OR, TOKEN_NOT

class PlainVar(Symbol):
    "Plain boolean variable"

class ColonDotVar(Symbol):
    "Colon and dot-separated string boolean variable"

class AdvancedAlgebra(BooleanAlgebra):
    def tokenize(self, expr):
        """
        Example custom tokenizer derived from the standard Python tokenizer
        with a few extra features: #-style comments are supported and a
        colon- and dot-separated string is recognized and stored in custom
        symbols. In contrast with the standard tokenizer, only these
        boolean operators are recognized : & | ! and or not.

        For more advanced tokenization you could also consider forking the
        `tokenize` standard library module.
        """

        if not isinstance(expr, basestring):
            raise TypeError('expr must be string but it is %s.' % type(expr))

        # mapping of lowercase token strings to a token object instance for
        # standard operators, parens and common true or false symbols
        TOKENS = {
            '&': TOKEN_AND,
            'and': TOKEN_AND,
            '|': TOKEN_OR,
            'or': TOKEN_OR,
            '!': TOKEN_NOT,
            'not': TOKEN_NOT,
            '(': TOKEN_LPAR,
            ')': TOKEN_RPAR,
            'true': TOKEN_TRUE,
            '1': TOKEN_TRUE,
            'false': TOKEN_FALSE,
            '0': TOKEN_FALSE,
            'none': TOKEN_FALSE,
        }

        ignored_token_types = (
            tokenize.NL, tokenize.NEWLINE, tokenize.COMMENT,
            tokenize.INDENT, tokenize.DEDENT,
            tokenize.ENDMARKER
        )

        # note: an unbalanced expression may raise a TokenError here.
        tokens = ((toktype, tok, row, col,) for toktype, tok, (row, col,), _, _
                  in tokenize.generate_tokens(StringIO(expr).readline)
                  if tok and tok.strip())

        COLON_DOT = (':', '.',)

        def build_symbol(current_dotted):
            if current_dotted:
                if any(s in current_dotted for s in COLON_DOT):
                    sym = ColonDotVar(current_dotted)
                else:
                    sym = PlainVar(current_dotted)
                return sym

        # accumulator for dotted symbols that span several `tokenize` tokens
        dotted, srow, scol = '', None, None

        for toktype, tok, row, col in tokens:
            if toktype in ignored_token_types:
                # we reached a break point and should yield the current dotted
                symbol = build_symbol(dotted)
                if symbol is not None:
                    yield symbol, dotted, (srow, scol)
                    dotted, srow, scol = '', None, None

                continue

            std_token = TOKENS.get(tok.lower())
            if std_token is not None:
                # we reached a break point and should yield the current dotted
                symbol = build_symbol(dotted)
                if symbol is not None:
                    yield symbol, dotted, (srow, scol)
                    dotted, srow, scol = '', 0, 0

                yield std_token, tok, (row, col)

                continue

            if toktype == tokenize.NAME or (toktype == tokenize.OP and tok in COLON_DOT):
                if not dotted:
                    srow = row
                    scol = col
                dotted += tok

            else:
                raise TypeError('Unknown token: %(tok)r at line: %(row)r, column: %(col)r' % locals())
