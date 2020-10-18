package main

import (
	"errors"
	"fmt"
	"strconv"
	"strings"
)

type tokenKind uint

const (
	numericKind tokenKind = iota
	operatorKind
	EOFKind
	keywordKind
	IDKind
	assignKind
	semiKind
	dotKind
	commaKind
	colonKind
)

func (e tokenKind) String() string {
	return [...]string{"numericKind",
		"operatorKind",
		"EOFKind",
		"keywordKind",
		"IDKind",
		"assignKind",
		"semiKind",
		"dotKind",
		"commaKind",
		"colonKind"}[e]
}

var (
	operatorList = []string{"+", "-", "*", "/", "(", ")"}
	keywordList  = []string{"BEGIN", "END", "PROCEDURE", "PROGRAM",
		"VAR", "DIV", "INTEGER", "REAL"}
)

type Token struct {
	Kind         tokenKind
	Value        string
	NumericValue interface{}
}

func isDigit(c byte) bool {
	return '0' <= c && c <= '9'
}

func isChar(c byte) bool {
	return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')
}

func isSpace(c byte) bool {
	return c == ' ' || c == '\n' || c == '\t'
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
	var token Token
	i := 0
	for i < len(text) {
		c := text[i]
		switch {
		case isDigit(c):
			j := i
			// multiple digits or float
			numericType := "INTEGER_CONST"
			for ; j < len(text) && (isDigit(text[j]) || text[j] == '.'); j++ {
				if text[j] == '.' {
					numericType = "REAL_CONST"
				}
			}

			s := text[i:j]
			switch numericType {
			case "INTEGER_CONST":
				v, err := strconv.Atoi(s)
				if err != nil {
					return tokens, err
				}
				token = Token{numericKind, "INTEGER_CONST", v}
			case "REAL_CONST":
				v, err := strconv.ParseFloat(s, 32)
				if err != nil {
					return tokens, err
				}
				token = Token{numericKind, "REAL_CONST", v}
			}
			i = j - 1
		case isSpace(c):
			i++
			continue
		case c == '{': // comment
			j := i
			for ; j < len(text) && text[j] != '}'; j++ {
			}
			i = j + 1
			continue
		case contains(operatorList, string(c)):
			token = Token{operatorKind, string(c), -1}
		case isChar(c):
			// multiple characters
			j := i
			for ; j < len(text) && (isDigit(text[j]) || isChar(text[j])); j++ {
			}
			s := text[i:j]
			if contains(keywordList, strings.ToUpper(s)) {
				if strings.ToUpper("s") == "DIV" {
					token = Token{operatorKind, "DIV", -1}
				} else {
					token = Token{keywordKind, strings.ToUpper(s), -1}
				}
			} else {
				token = Token{IDKind, s, -1}
			}
			i = j - 1
		case c == ':' && text[i+1] == '=':
			token = Token{assignKind, ":=", -1}
			i++
		case c == ':' && text[i+1] != '=':
			token = Token{colonKind, ":", -1}
		case c == ';':
			token = Token{semiKind, ";", -1}
		case c == '.':
			token = Token{dotKind, ".", -1}
		case c == ',':
			token = Token{commaKind, ",", -1}
		default:
			fmt.Println(string(c))
			return tokens, errors.New("Error lexing input")
		}

		tokens = append(tokens, token)
		i++
	}

	tokens = append(tokens,
		Token{EOFKind, "", -1})
	return tokens, nil
}
