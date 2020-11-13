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
	KeywordNotFound      = "Keyword not found"
	DuplicateID          = "Duplicate ID found"
	UnknownType          = "Unknown node type"
)

type ErrorType string

const (
	LexerErrorType    = "Lexer"
	ParserErrorType   = "Parser"
	SemanticErrorType = "Semantic"
)

type LexerError struct {
	Code   ErrorCode
	Token  Token
	Lexeme string
}

func (e *LexerError) Error() string {
	return fmt.Sprintf("[Lexer] %s '%s' <%d:%d>", e.Code, e.Lexeme, e.Token.Lineno, e.Token.Column)
}

type ParserError struct {
	Code  ErrorCode
	Token Token
}

func (e *ParserError) Error() string {
	return fmt.Sprintf("[Parser] %s: %v", e.Code, e.Token)
}

type SemanticError struct {
	Code  ErrorCode
	Token Token
}

func (e *SemanticError) Error() string {
	return fmt.Sprintf("[Semantic] %s: %v", e.Code, e.Token)
}

type CompilerError struct {
	Code  ErrorCode
	Token Token
}

func (e *CompilerError) Error() string {
	return fmt.Sprintf("[Compiler] %s: %v", e.Code, e.Token)
}

func do(text string) (interface{}, error) {
	// lexing: decompose string into tokens
	// also convert tokens into values based on their kinds
	tokens, err := lex(text)
	if err != nil {
		return nil, err
	}
	// for _, token := range tokens {
	// 	fmt.Println(token)
	// }

	// parser: build AST representation
	parser := NewParser(tokens)
	node, err := parser.parse()
	if err != nil {
		return nil, err
	}
	draw(node, "")

	// semantic checking
	semanticAnalyzer := NewSemanticAnalyzer()
	err = semanticAnalyzer.visit(node)
	if err != nil {
		return nil, err
	}

	// source-to-source compiler
	// src2srcCompiler := NewSourceToSourceCompiler()
	// s, err := src2srcCompiler.visit(node)
	// if err != nil {
	// 	return nil, err
	// }
	// fmt.Println("------ Compiler -----------")
	// fmt.Println(s)

	// interpreter: generate result
	itpr := Interpreter{node: node, globalScope: make(map[string]interface{})}
	_, err = itpr.interprete()
	if err != nil {
		return nil, err
	}
	fmt.Println(itpr.globalScope)

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
