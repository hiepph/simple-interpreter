package main

import (
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
	Lineno       int
	Column       int
}

func (t Token) String() string {
	return fmt.Sprintf("[%s] %s (%d) <%d:%d>", t.Kind, t.Value, t.NumericValue, t.Lineno, t.Column)
}

func isDigit(c byte) bool {
	return '0' <= c && c <= '9'
}

func isChar(c byte) bool {
	return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')
}

func isSpace(c byte) bool {
	return c == ' ' || c == '\t'
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

	var column = 0 // 0-index
	var lineno = 1 // 1-index
	i := 0
	for i < len(text) {
		c := text[i]
		switch {
		case isDigit(c):
			var startColumn = column
			j := i
			// multiple digits or float
			numericType := "INTEGER_CONST"
			for ; j < len(text) && (isDigit(text[j]) || text[j] == '.'); j++ {
				if text[j] == '.' {
					numericType = "REAL_CONST"
				}
				column++
			}

			s := text[i:j]
			switch numericType {
			case "INTEGER_CONST":
				v, err := strconv.Atoi(s)
				if err != nil {
					return tokens, err
				}
				token = Token{numericKind, "INTEGER_CONST", v, lineno, startColumn}
			case "REAL_CONST":
				v, err := strconv.ParseFloat(s, 32)
				if err != nil {
					return tokens, err
				}
				token = Token{numericKind, "REAL_CONST", v, lineno, startColumn}
			}
			i = j - 1
		case contains(operatorList, string(c)):
			token = Token{operatorKind, string(c), -1, lineno, column}
		case isChar(c):
			var startColumn = column
			// multiple characters
			j := i
			for ; j < len(text) && (isDigit(text[j]) || isChar(text[j])); j++ {
				column++
			}
			s := text[i:j]
			if contains(keywordList, strings.ToUpper(s)) {
				if strings.ToUpper("s") == "DIV" {
					token = Token{operatorKind, "DIV", -1, lineno, startColumn}
				} else {
					token = Token{keywordKind, strings.ToUpper(s), -1, lineno, startColumn}
				}
			} else {
				token = Token{IDKind, s, -1, lineno, startColumn}
			}
			i = j - 1
			column--
		case c == '{': // comment
			j := i
			for ; j < len(text) && text[j] != '}'; j++ {
				column++
			}
			i = j + 1
			continue
		case c == ':' && text[i+1] == '=':
			token = Token{assignKind, ":=", -1, lineno, column}
			i++
			column++
		case c == ':' && text[i+1] != '=':
			token = Token{colonKind, ":", -1, lineno, column}
		case c == ';':
			token = Token{semiKind, ";", -1, lineno, column}
		case c == '.':
			token = Token{dotKind, ".", -1, lineno, column}
		case c == ',':
			token = Token{commaKind, ",", -1, lineno, column}
		case c == '\n':
			i++
			lineno += 1
			column = 0
			continue
		case isSpace(c):
			i++
			column++
			continue
		default:
			fmt.Println(string(c))
			return tokens, &LexerError{UnexpectedTokenError, token, string(c)}
		}

		tokens = append(tokens, token)
		i++
		column++
	}

	// tokens = append(tokens,
	// 	Token{EOFKind, "", -1, lineno, column})
	return tokens, nil
}
