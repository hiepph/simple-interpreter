package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
)

type ErrorCode string

const (
	UnexpectedTokenError = "Unexpected Token"
	IdNotFound           = "Identifier not found"
	DuplicateID          = "Duplicate ID found"
)

type Error struct {
	ErrorCode ErrorCode
	Token     Token
	Msg       string
}

func (e *Error) Error() string {
	return fmt.Sprintf("%s", e.Msg)
}

func do(text string) (interface{}, error) {
	// 1. lexing: decompose string into tokens
	// also convert tokens into values based on their kinds
	tokens, err := lex(text)
	if err != nil {
		return nil, err
	}
	// for i, token := range tokens {
	// 	fmt.Println(i, token)
	// }

	// 2. parser: build AST representation
	parser := NewParser(tokens)
	node, err := parser.parse()
	if err != nil {
		return nil, err
	}
	draw(node, "")

	// 3. interpreter: generate result
	// semanticAnalyzer := NewSemanticAnalyzer()
	// err = semanticAnalyzer.visit(node)
	// if err != nil {
	// 	return nil, err
	// }
	// fmt.Println("------ SymbolTable --------")
	// fmt.Println(semanticAnalyzer.Table)

	// source-to-source compiler
	src2srcCompiler := NewSourceToSourceCompiler()
	s, err := src2srcCompiler.visit(node)
	if err != nil {
		return nil, err
	}
	fmt.Println("------ Compiler -----------")
	fmt.Println(s)

	// itpr := Interpreter{node: node, globalScope: make(map[string]interface{})}
	// _, err = itpr.interprete()
	// if err != nil {
	// 	return nil, err
	// }

	return nil, nil
}

func main() {
	content, err := ioutil.ReadFile(os.Args[1])
	if err != nil {
		log.Fatal(err)
	}
	_, err = do(string(content))

	if err != nil {
		log.Fatal(err)
	}
}
