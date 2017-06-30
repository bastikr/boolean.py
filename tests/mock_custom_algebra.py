from boolean import BooleanAlgebra, Symbol

from boolean import TOKEN_SYMBOL
from boolean import TOKEN_LPAR, TOKEN_RPAR
from boolean import TOKEN_AND, TOKEN_OR, TOKEN_NOT

class CustomSymbol(Symbol):
    pass

class CustomAlgebra(BooleanAlgebra):
    def __init__(self, Symbol_class=CustomSymbol):
        super(CustomAlgebra, self).__init__(Symbol_class=CustomSymbol)

    def tokenize(self, s):
        "Sample tokenizer using custom operators and symbols"
        ops = {
            'WHY_NOT': TOKEN_OR,
            'ALSO': TOKEN_AND,
            'NEITHER': TOKEN_NOT,
            '(': TOKEN_LPAR,
            ')': TOKEN_RPAR,
        }

        for row, line in enumerate(s.splitlines(False)):
            for col, tok in enumerate(line.split()):
                if tok in ops:
                    yield ops[tok], tok, (row, col)
                elif tok == 'Custom':
                    yield self.Symbol(tok), tok, (row, col)
                else:
                    yield TOKEN_SYMBOL, tok, (row, col)
