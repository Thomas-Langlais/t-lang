# Things that need to be implemented
* a lexer needs to parse a raw byte stream and creates a list of tokens that contain the lex token type, value, and position in the byte stream
* a parser that converts the tokens into a parse tree that can be interpreted
* an interpreter to execute the parse tree

## The lexer
The lexer needs to be able to understand the lexemes that it's allowed to parse into the token list.
To start, I'll just to arithmetic operations like `1 - 2`, `1 / 2`, `2 + 3 * 2`, and `(2 + 3) * 2` following BEDMAS operation order.

## The parser
The parser must follow a finite grammar in order to deterministicly calculate the parse tree

WIP