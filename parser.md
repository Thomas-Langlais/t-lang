# Grammar rules
operation = PLUS|MINUS|MUL|DIV
factor = INT|FLOAT
term = factor (operation factor)*
expression = term (operation term)*