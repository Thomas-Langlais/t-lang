# Grammar rules
operator = PLUS|MINUS|MUL|DIV
factor = INT|FLOAT
term = factor (operator factor)*
expression = term (operator term)*