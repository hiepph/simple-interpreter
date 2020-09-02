package main

import (
	"errors"
	"fmt"
	"log"
	"strconv"
)

type tokenKind uint

const (
	numericKind tokenKind = iota
	operatorKind
	EOFKind
)

type Operator string

const (
	PLUS   Operator = "PLUS"
	MINUS  Operator = "MINUS"
	MUL    Operator = "MUL"
	DIV    Operator = "DIV"
	LPAREN Operator = "LPAREN"
	RPAREN Operator = "RPAREN"
)

var operatorDictionary = map[string]Operator{
	"+": PLUS,
	"-": MINUS,
	"*": MUL,
	"/": DIV,
	"(": LPAREN,
	")": RPAREN,
}

type Token struct {
	kind     tokenKind
	value    string
	intValue int
}

type AST interface {
}

type BinOp struct {
	Left  AST
	Op    Token
	Right AST
}

type Num struct {
	Token Token
	Value int
}

func NewNum(token Token) Num {
	num := Num{Token: token}
	num.Value = token.intValue
	return num
}

func isDigit(c byte) bool {
	return '0' <= c && c <= '9'
}

func isSpace(c byte) bool {
	return c-' ' == 0
}

func isOperator(c byte) bool {
	_, found := operatorDictionary[string(c)]
	return found
}

func contains(arr []string, s string) bool {
	for _, el := range arr {
		if el == s {
			return true
		}
	}

	return false
}

func lex(text string) ([]Token, error) {
	tokens := make([]Token, 0)
	i := 0
	for i < len(text) {
		c := text[i]
		switch {
		case isDigit(c):
			j := i
			// multiple-digits
			for ; j < len(text) && isDigit(text[j]); j++ {
			}

			tokens = append(tokens,
				Token{numericKind, text[i:j], -1})
			i = j
		case isSpace(c):
			i++
		case isOperator(c):
			tokens = append(tokens,
				Token{operatorKind, string(c), -1})
			i++
		default:
			return tokens, errors.New("Error lexing input")
		}
	}

	return tokens, nil
}

func parse(tokens []Token) ([]Token, error) {
	for i, token := range tokens {
		if token.kind == numericKind {
			value, err := strconv.Atoi(token.value)
			if err != nil {
				return tokens, errors.New("Error parsing tokens")
			}

			tokens[i].intValue = value
		} else if token.kind == operatorKind {
			op, _ := operatorDictionary[token.value]
			tokens[i].value = string(op)
		}
	}

	return tokens, nil
}

/////// INTERPRETER
type Interpreter struct {
	tokens []Token
	cur    int
}

func (itpr Interpreter) currentToken() Token {
	if itpr.cur >= len(itpr.tokens) {
		return Token{EOFKind, "", -1}
	}
	return itpr.tokens[itpr.cur]
}

func (itpr *Interpreter) eat(kind tokenKind, value string) error {
	token := itpr.currentToken()
	if (kind == numericKind && token.kind == kind) ||
		(token.kind == kind && token.value == value) {
		itpr.cur++
		return nil
	}

	return errors.New(fmt.Sprintf("Error eating tokens %d."+
		"want: %v, actual: %v", itpr.cur, value, token.value))
}

func (itpr *Interpreter) factor() (AST, error) {
	token := itpr.currentToken()

	switch token.kind {
	case numericKind:
		err := itpr.eat(numericKind, "INTEGER")
		if err != nil {
			return nil, err
		}
		return NewNum(token), nil
	case operatorKind:
		if token.value == "LPAREN" {
			err := itpr.eat(operatorKind, "LPAREN")
			if err != nil {
				return nil, err
			}
			node, err := itpr.expr()
			if err != nil {
				return nil, err
			}
			err = itpr.eat(operatorKind, "RPAREN")
			if err != nil {
				return nil, err
			}
			return node, nil
		}
	}

	return nil, errors.New("Error factor")
}

func (itpr *Interpreter) term() (AST, error) {
	node, err := itpr.factor()
	if err != nil {
		return nil, err
	}

	for itpr.currentToken().kind == operatorKind &&
		contains([]string{"MUL", "DIV"}, itpr.currentToken().value) {
		token := itpr.currentToken()
		switch token.value {
		case "MUL":
			err := itpr.eat(operatorKind, "MUL")
			if err != nil {
				return nil, err
			}
		case "DIV":
			err := itpr.eat(operatorKind, "DIV")
			if err != nil {
				return nil, err
			}
		}
		rightNode, err := itpr.factor()
		if err != nil {
			return nil, err
		}
		node = BinOp{Left: node, Op: token, Right: rightNode}
	}

	return node, nil
}

func (itpr *Interpreter) expr() (AST, error) {
	// expr: term ((MUL|DIV)term)*
	// term: factor ((MUL|DIV)factor)*
	// factor: INTEGER | LPAREN expr RPAREN
	node, err := itpr.term()
	if err != nil {
		return nil, err
	}

	for itpr.currentToken().kind == operatorKind &&
		contains([]string{"PLUS", "MINUS"}, itpr.currentToken().value) {
		token := itpr.currentToken()
		switch token.value {
		case "PLUS":
			err := itpr.eat(operatorKind, "PLUS")
			if err != nil {
				return nil, err
			}
		case "MINUS":
			err := itpr.eat(operatorKind, "MINUS")
			if err != nil {
				return nil, err
			}
		}
		rightNode, err := itpr.term()
		if err != nil {
			return nil, err
		}
		node = BinOp{Left: node, Op: token, Right: rightNode}
	}

	return node, nil
}

func visitBinOp(node AST) (int, error) {
	nodeBinOp := node.(BinOp)

	left, err := visit(nodeBinOp.Left)
	if err != nil {
		return -1, err
	}
	right, err := visit(nodeBinOp.Right)
	if err != nil {
		return -1, err
	}

	switch nodeBinOp.Op.value {
	case "PLUS":
		return left + right, nil
	case "MINUS":
		return left - right, nil
	case "MUL":
		return left * right, nil
	case "DIV":
		return left / right, nil
	default:
		return -1, errors.New("Unkown Op")
	}
}

func visitNum(node AST) (int, error) {
	return node.(Num).Value, nil
}

func visit(node AST) (int, error) {
	switch node.(type) {
	case BinOp:
		return visitBinOp(node)
	case Num:
		return visitNum(node)
	default:
		return -1, errors.New("Unknown node type")
	}
}

///// main flow
func interprete(text string) (AST, error) {
	// 1. lexing: decompose string into tokens
	tokens, err := lex(text)
	if err != nil {
		return -1, err
	}

	// 2. parser: convert tokens into values based on their kinds
	tokens, err = parse(tokens)
	if err != nil {
		return -1, err
	}

	// 3. interpreter: generate result
	interpreter := Interpreter{tokens, 0}
	return interpreter.expr()
}

func main() {
	node, err := interprete("7 + 3 * (10 / (12 / (3 + 1) - 1))")
	// node, err := interprete("2 * 7 + 3")
	if err != nil {
		log.Fatal(err)
	}
	fmt.Printf("%+v\n", node)

	v, err := visit(node)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(v)
}
