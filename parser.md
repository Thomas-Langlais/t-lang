# Grammar rules
expression = term (PLUS|MINUS term)*
term       = factor (MUL|DIV factor)*
factor     = INT|FLOAT
           = LParen expression RParen