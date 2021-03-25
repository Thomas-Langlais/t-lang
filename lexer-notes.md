# Lexer Notes
The anatomy of the token vector that gets generated is broken down to the following rules.

## Expectations
### Operating on integers
```
input = "1 + 3"
output = TOKEN:INT(1) TOKEN:PLUS TOKEN:INT(3)

input = "1 * 3"
output = TOKEN:INT(1) TOKEN:MUL TOKEN:INT(3)
```

### Operating on floats
```
input = "1.2 / 3.5"
output = TOKEN:FLOAT(1.2) TOKEN:DIV TOKEN:FLOAT(3.5)

input = "1.2 - 3.5"
output = TOKEN:FLOAT(1.2) TOKEN:MINUS TOKEN:FLOAT(3.5)
```

### Order of operation brackets
```
input = "(1 + 3) * 2"
output = TOKEN:LPAREN('(') TOKEN:INT(1) TOKEN:PLUS TOKEN:INT(3) TOKEN:RPAREN(')') TOKEN:MUL TOKEN:INT(2)
```

## Outcomes
* The lexer shall produce the same tokens regardless of whitespace placement.
* The system currently only evaluates one line at a time. this will change when the language adapts

# Plan
- [ ] Create a unit testing module
- [ ] Create unit tests for the following
    - [ ] Operators
        - [ ] Multiplication
        - [ ] Division
        - [ ] Addition
        - [ ] Subtraction
    - [ ] Operands
        - [ ] Integers
        - [ ] Floating point numbers
    - [ ] Parenthesies
        - [ ] Left-hand parenthesies
        - [ ] Right-hand parenthesies