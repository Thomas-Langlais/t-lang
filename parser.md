# Notes
KW is short for Keyword, so the grammar component after this prefix is referring to an item in this keyword list

the concept of an atom - from my understanding is a symbol that can be any combination of words that make of the english (or any really) language.
This is used to restrict what is allowed to be used for programming purposes. 
[link](http://www.cburch.com/cs/150/reading/grammar/index.html)

# Grammar rules
```
statement  = fn_stmt
           = for_stmt
           = while_stmt
           = if_stmt
           = decl_stmt LINETERM
           = KW:CONTINUE LINETERM
           = KW:BREAK LINETERM
           = KW:RETURN expr? LINETERM
           = expr LINETERM

fn_stmt    = KW:FUN IDENTIFIER
              LParen expr? (COMMA expr)* RParen
              block

if_stmt    = KW:IF expr block
                (KW:ELSE KW:IF expr block)*
              | (KW:ELSE block)?

for_stmt   = KW:FOR LParen (decl_stmt)? LINETERM
               (expr)? LINETERM
               (decl_stmt)?
             RParen block

while_stmt = KW:WHILE expr block

decl_stmt  = KW:LET IDENTIFIER EQ expr

block      = LBlock statement+ RBlock

expr       = comp_expr ((AND|OR) comp_expr)*

comp_expr  = NOT comp_expr
           = arith_expr ((EE|NE|LT|GT|LTE|GTE) arith_expr)*

arith_expr = term (PLUS|MINUS term)*

term       = factor (MUL|DIV factor)*

factor     = atom
           = (PLUS|MINUS) factor

atom       = INT|FLOAT
           = IDENTIFIER (LParen expr? (COMMA expr)* RParen)?
           = LParen expr RParen
```

           
# Token type constant values appendix
| token | text |
| - | - |
| EQ | `=` |
| NOT | `!` |
| AND | `&&` |
| OR | `\|\|` |
| EE | `==` |
| NE | `!=` |
| LT | `<` |
| GT | `>` |
| LTE | `<=` |
| GTE | `>=` |
| LBlock | `\|-` |
| RBlock | `-\|` |
