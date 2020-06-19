package main

import (
	"errors"
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

var operatorDictionary = map[byte]Operator{
	'+': PLUS,
	'-': MINUS,
}

type Token struct {
	kind  tokenKind
	value string
}

func isDigit(c byte) bool {
	return '0' <= c && c <= '9'
}

func isSpace(c byte) bool {
	return c-' ' == 0
}

func isOperator(c byte) bool {
	_, found := operatorDictionary[c]
	return found
}

func toInteger(s string) int {
	v, _ := strconv.Atoi(s)
	return v
}

func expr(text string) (int, error) {
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

			tokens = append(tokens, Token{numericKind, text[i:j]})
			i = j
		case isSpace(c):
			i++
		case isOperator(c):
			op, _ := operatorDictionary[c]
			tokens = append(tokens, Token{operatorKind, string(op)})
			i++
		default:
			return -1, errors.New("Error parsing input")
		}

	}

	patterns := []tokenKind{numericKind, operatorKind, numericKind}
	for i, _ := range tokens {
		if tokens[i].kind != patterns[i] {
			return -1, errors.New("Error parsing patterns")
		}
	}

	switch tokens[1].value {
	case "PLUS":
		return toInteger(tokens[0].value) + toInteger(tokens[2].value), nil
	case "MINUS":
		return toInteger(tokens[0].value) - toInteger(tokens[2].value), nil
	}

	return -1, errors.New("Error parsing values")
}

func main() {
	_, err := expr("13 + 5")
	if err != nil {
		log.Fatal(err)
	}
}
