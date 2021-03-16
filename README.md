# T-Lang
I wanted to challenge myself into deepening my understanding on how high-level languages like: Ruby, Python, and Java interpret source code; as well as, the mechanisms to run the code or compile the code into an executable file.

For now, I will learn to create an interpreter as it's a good starting point.
I would like to learn how to convert the AST into an executable file.

## Goals
* I want to learn a modern functional programming language because I do not have any experience using functional languages.
I chose to learn Rust as it's new language, open-source, and fast. I chose Rust over Golang because in almost every metric Rust is better, but a concequence is that it requires more memory management and C-like nuances.
* Learn how to create a language. There is a use case for work where creating a custom query language to escape the pitfalls of the current data structure.
This will give me the basics to creating an AST from a string and validate whether the code is well-formed

## Components
Every programming language has the following components:
* Lexer
* Parser
* Interpreter/Assembler

### Lexer
The component that breaks down an input string into smaller units (aka tokens) that the language understands

### Parser
The component that analyzes the lexeme tokens generated and forms a syntax that conforms to the language grammar and creates the AST.

### Interpreter/Assembler
Uses the AST from the parser to run the program that uses the language or generate machine code that is equivalent to the source code.