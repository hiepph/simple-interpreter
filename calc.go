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
)

type Operator string

const (
	PLUS  Operator = "PLUS"
	MINUS Operator = "MINUS"
)

var operatorDictionary = map[string]Operator{
	"+": PLUS,
	"-": MINUS,
}

type Token struct {
	kind     tokenKind
	value    string
	intValue int
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

func expr(text string) (int, error) {
	// 1. lexing: decompose string into tokens
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
			return -1, errors.New("Error lexing input")
		}

	}

	// 2. parser: convert tokens into values based on their kinds
	for i, token := range tokens {
		if token.kind == numericKind {
			value, err := strconv.Atoi(token.value)
			if err != nil {
				return -1, errors.New("Error parsing tokens")
			}

			tokens[i].intValue = value
		} else if token.kind == operatorKind {
			op, _ := operatorDictionary[token.value]
			tokens[i].value = string(op)
		}
	}

	// 3. interpreter: generate result
	result := tokens[0].intValue
	for i := 1; i < len(tokens); {
		if tokens[i].kind == operatorKind {
			switch tokens[i].value {
			case "PLUS":
				result += tokens[i+1].intValue
			case "MINUS":
				result -= tokens[i+1].intValue
			}

			i += 2
		} else {
			i++
		}
	}

	return result, nil
}

func main() {
	v, err := expr("13 + 5 - 12")
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(v)
}
