A simple Pascal interpreter written in Go.

This implementation is currently a work-in-progress. It's written with
the mindset of learning both Go and the under-the-hood theory of interpreter.

This implementation is based on the series by
[Ruslan](https://ruslanspivak.com/lsbasi-part1/) which was originally
written in Python.
It started in 2015 and by Nov 2020 (the time I finished part 19)
it doesn't seem to end yet.
Kudos to the author for the excellent series by the way.

## How to run

Simply invoke the program with:

```
go run . samples/part19.pas
```

## Main

`si.go`

Main progress resides here: reading source file, lexing, parsing,
semantic checking, and lastly interpreting.

## Lexer

`lexer.go`

In this part, I don't follow the author by making a `Lexer` class
but traversing the code to build tokens manually.
The intention is the same: breaking down the source code into
individual tokens with values and types.

## Parser

`paser.go`

Build an AST (Abstract Syntax Tree) representation from tokens.
The grammar is based on [EBNF](https://www.wikiwand.com/en/Extended_Backus%E2%80%93Naur_form).

## Semantic Analyzer

`semantic_analyzer.go`

Check the syntax, collect information from the AST representation.

## Interpreter

`interpreter.go`

From a validated AST, visit each node and evaluate them.
