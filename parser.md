# Notes
KW is short for Keyword, so the grammar component after this prefix is referring to an item in this keyword list

the concept of an atom - from my understanding is a symbol that can be any combination of words that make of the english (or any really) language.
This is used to restrict what is allowed to be used for programming purposes. 
[link](http://www.cburch.com/cs/150/reading/grammar/index.html)

# Grammar rules
```
expression = KW:LET IDENTIFIER EQ expr 
           = expr

expr       = comp_expr ((AND|OR) comp_expr)*

comp_expr  = NOT comp_expr
           = arith_expr ((EE|NE|LT|GT|LTE|GTE) arith_expr)*

arith_expr = term (PLUS|MINUS term)*

term       = factor (MUL|DIV factor)*

factor     = atom
           = (PLUS|MINUS) factor

atom       = INT|FLOAT|IDENTIFIER
           = LParen expression RParen
```

           
# Token type constant values appendix
| token | text |
| - | - |
| EQ | `=` |
| NOT | `!` |
| AND | `&&` |
| OR | `||` |
| EE | `==` |
| NE | `!=` |
| LT | `<` |
| GT | `>` |
| LTE | `<=` |
| GTE | `>=` |
