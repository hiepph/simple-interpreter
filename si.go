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

type Operator string

const (
	PLUS      Operator = "PLUS"
	MINUS     Operator = "MINUS"
	MUL       Operator = "MUL"
	DIV       Operator = "DIV"
	FLOAT_DIV Operator = "FLOAT_DIV"
	LPAREN    Operator = "LPAREN"
	RPAREN    Operator = "RPAREN"
)

var operatorDictionary = map[string]Operator{
	"+": PLUS,
	"-": MINUS,
	"*": MUL,
	"/": FLOAT_DIV,
	"(": LPAREN,
	")": RPAREN,
}

var keywordList = []string{"BEGIN", "END", "PROGRAM", "VAR", "DIV", "INTEGER", "REAL"}

type Token struct {
	Kind         tokenKind
	Value        string
	NumericValue interface{}
}

type AST interface {
}

type Program struct {
	Name  string
	Block Block
}

type Block struct {
	Declarations      []VarDecl
	CompoundStatement AST
}

type VarDecl struct {
	VarNode  AST
	TypeNode AST
}

type Type struct {
	Token Token
	Value interface{}
}

type BinOp struct {
	Left  AST
	Op    Token
	Right AST
}

type UnaryOp struct {
	Op   Token
	expr AST
}

type Num struct {
	Token Token
	Value interface{}
}

func NewNum(token Token) Num {
	num := Num{Token: token}
	num.Value = token.NumericValue
	return num
}

type Compound struct {
	Children []AST
}

type Assign struct {
	Left  AST
	Op    Token
	Right AST
}

type Var struct {
	Token Token
	Value string
}

func NewVar(token Token) Var {
	v := Var{Token: token}
	v.Value = token.Value
	return v
}

type NoOp struct{}

func isDigit(c byte) bool {
	return '0' <= c && c <= '9'
}

func isChar(c byte) bool {
	return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')
}

func isSpace(c byte) bool {
	return c == ' ' || c == '\n' || c == '\t'
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

/////// LEXER
func lex(text string) ([]Token, error) {
	tokens := make([]Token, 0)
	var token Token
	i := 0
	for i < len(text) {
		c := text[i]
		// fmt.Println(i, string(c))
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
		case isOperator(c):
			op, _ := operatorDictionary[string(c)]
			token = Token{operatorKind, string(op), -1}
		case isChar(c):
			// multiple characters
			j := i
			for ; j < len(text) && (isDigit(text[j]) || isChar(text[j])); j++ {
			}
			s := text[i:j]
			if contains(keywordList, s) {
				if s == "DIV" {
					token = Token{operatorKind, "DIV", -1}
				} else {
					token = Token{keywordKind, s, -1}
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

/////// PARSER
type Parser struct {
	tokens []Token
	cur    int
}

func NewParser(tokens []Token) Parser {
	return Parser{tokens: tokens, cur: 0}
}

func (parser Parser) currentToken() Token {
	if parser.cur >= len(parser.tokens) {
		return Token{EOFKind, "", -1}
	}
	return parser.tokens[parser.cur]
}

func (parser *Parser) eatOnlyKind(kind tokenKind) error {
	token := parser.currentToken()
	if token.Kind == kind {
		parser.cur++
		return nil
	}
	return errors.New(fmt.Sprintf("Error eating tokens %d: %v. "+
		"want: %v, actual: %v", parser.cur, parser.tokens[parser.cur], kind, token.Kind))
}

func (parser *Parser) eat(kind tokenKind, value string) error {
	token := parser.currentToken()
	if token.Kind == kind && token.Value == value {
		parser.cur++
		return nil
	}

	return errors.New(fmt.Sprintf("Error eating tokens %d: %v.\n"+
		"> Kind, want: %v, actual: %v\n"+
		"> Value, want: %v, actual: %v\n",
		parser.cur, parser.tokens[parser.cur],
		kind, token.Kind,
		value, token.Value))
}

func (parser *Parser) program() (AST, error) {
	// program: PROGRAM variable SEMI block DOT
	err := parser.eat(keywordKind, "PROGRAM")
	if err != nil {
		return nil, err
	}

	varNode, err := parser.variable()
	if err != nil {
		return nil, err
	}
	progName := varNode.(Var).Value

	err = parser.eatOnlyKind(semiKind)
	blockNode, err := parser.block()
	if err != nil {
		return nil, err
	}

	programNode := Program{progName, blockNode.(Block)}
	err = parser.eatOnlyKind(dotKind)
	if err != nil {
		return nil, err
	}
	return programNode, nil
}

func (parser *Parser) compoundStatement() (AST, error) {
	// compoundStatement: BEGIN statementList END
	err := parser.eat(keywordKind, "BEGIN")
	if err != nil {
		return nil, err
	}
	nodes, err := parser.statementList()
	if err != nil {
		return nil, err
	}

	err = parser.eat(keywordKind, "END")
	if err != nil {
		return nil, err
	}

	root := Compound{}
	for _, node := range nodes {
		root.Children = append(root.Children, node)
	}
	return root, nil
}

func (parser *Parser) statementList() ([]AST, error) {
	// statementList: statement
	//              | statement SEMI statementList
	node, err := parser.statement()
	if err != nil {
		return nil, err
	}

	results := []AST{node}
	for parser.currentToken().Kind == semiKind {
		err = parser.eatOnlyKind(semiKind)
		if err != nil {
			return nil, err
		}
		node, err := parser.statement()
		if err != nil {
			return nil, err
		}
		results = append(results, node)
	}

	// ?
	// if parser.currentToken().Kind == IDKind {
	// 	return nil, errors.New()
	// }

	return results, nil
}

func (parser *Parser) statement() (AST, error) {
	// statement: compoundStatement
	//          | assignStatement
	//          | empty
	token := parser.currentToken()
	switch {
	case token.Kind == keywordKind && token.Value == "BEGIN":
		return parser.compoundStatement()
	case token.Kind == IDKind:
		return parser.assignStatement()
	default:
		return parser.empty()
	}
}

func (parser *Parser) assignStatement() (AST, error) {
	// assignStatement: variable ASSIGN expr
	left, err := parser.variable()
	token := parser.currentToken()
	if err != nil {
		return nil, err
	}
	err = parser.eatOnlyKind(assignKind)
	if err != nil {
		return nil, err
	}
	right, err := parser.expr()
	if err != nil {
		return nil, err
	}

	return Assign{Left: left, Op: token, Right: right}, nil
}

func (parser *Parser) variable() (AST, error) {
	// variable: ID
	node := NewVar(parser.currentToken())
	err := parser.eatOnlyKind(IDKind)
	return node, err
}

func (parser *Parser) empty() (AST, error) {
	return NoOp{}, nil
}

func (parser *Parser) factor() (AST, error) {
	// factor : PLUS factor
	//        | MINUS factor
	//        | INTEGER_CONST
	//        | REAL_CONST
	//        | LPAREN expr RPAREN
	//        | variable
	token := parser.currentToken()

	var err error
	switch token.Kind {
	case numericKind:
		if token.Value == "INTEGER_CONST" {
			err = parser.eat(numericKind, "INTEGER_CONST")
		} else if token.Value == "REAL_CONST" {
			err = parser.eat(numericKind, "REAL_CONST")
		}
		if err != nil {
			return nil, err
		}
		return NewNum(token), nil
	case operatorKind:
		if token.Value == "LPAREN" {
			err = parser.eat(operatorKind, "LPAREN")
			if err != nil {
				return nil, err
			}
			node, err := parser.expr()
			if err != nil {
				return nil, err
			}
			err = parser.eat(operatorKind, "RPAREN")
			if err != nil {
				return nil, err
			}
			return node, nil
		} else if token.Value == "PLUS" || token.Value == "MINUS" {
			err = parser.eat(operatorKind, token.Value)
			fact, err := parser.factor()
			if err != nil {
				return nil, err
			}
			node := UnaryOp{Op: token, expr: fact}
			return node, nil
		}
	default:
		return parser.variable()
	}

	return nil, errors.New("Error factor")
}

func (parser *Parser) term() (AST, error) {
	// term : factor ((MUL | DIV | FLOAT_DIV) factor)*
	var err error

	node, err := parser.factor()
	if err != nil {
		return nil, err
	}

	for parser.currentToken().Kind == operatorKind &&
		contains([]string{"MUL", "DIV", "FLOAT_DIV"}, parser.currentToken().Value) {
		token := parser.currentToken()
		switch token.Value {
		case "MUL":
			err = parser.eat(operatorKind, "MUL")
		case "DIV":
			err = parser.eat(operatorKind, "DIV")
		case "FLOAT_DIV":
			err = parser.eat(operatorKind, "FLOAT_DIV")
		}
		if err != nil {
			return nil, err
		}

		rightNode, err := parser.factor()
		if err != nil {
			return nil, err
		}
		node = BinOp{Left: node, Op: token, Right: rightNode}
	}

	return node, nil
}

func (parser *Parser) expr() (AST, error) {
	// expr: term ((MUL|DIV)term)*
	// term: factor ((MUL|DIV)factor)*
	// factor: (PLUS|MINUS) factor | INTEGER | LPAREN expr RPAREN
	node, err := parser.term()
	if err != nil {
		return nil, err
	}

	for parser.currentToken().Kind == operatorKind &&
		contains([]string{"PLUS", "MINUS"}, parser.currentToken().Value) {
		token := parser.currentToken()
		switch token.Value {
		case "PLUS":
			err := parser.eat(operatorKind, "PLUS")
			if err != nil {
				return nil, err
			}
		case "MINUS":
			err := parser.eat(operatorKind, "MINUS")
			if err != nil {
				return nil, err
			}
		}
		rightNode, err := parser.term()
		if err != nil {
			return nil, err
		}
		node = BinOp{Left: node, Op: token, Right: rightNode}
	}

	return node, nil
}

func (parser *Parser) block() (AST, error) {
	// block: declarations compound_statement
	declarationNodes, err := parser.declarations()
	if err != nil {
		return -1, err
	}
	compoundStatementNode, err := parser.compoundStatement()
	if err != nil {
		return -1, err
	}
	return Block{declarationNodes, compoundStatementNode}, nil
}

func (parser *Parser) declarations() ([]VarDecl, error) {
	// declarations: VAR (variable_declaration SEMI)+
	//             | empty
	var result []VarDecl
	currentToken := parser.currentToken()
	if currentToken.Kind == keywordKind && currentToken.Value == "VAR" {
		err := parser.eat(keywordKind, "VAR")
		if err != nil {
			return []VarDecl{}, err
		}
		for parser.currentToken().Kind == IDKind {
			varDecls, err := parser.variableDeclarations()
			if err != nil {
				return []VarDecl{}, err
			}
			for _, varDecl := range varDecls {
				result = append(result, varDecl)
			}
			err = parser.eatOnlyKind(semiKind)
			if err != nil {
				return []VarDecl{}, err
			}
		}
	}

	return result, nil
}

func (parser *Parser) variableDeclarations() ([]VarDecl, error) {
	// variable_declaration: ID (COMMA ID)* COLON type_spec
	varNodes := []AST{NewVar(parser.currentToken())}
	err := parser.eatOnlyKind(IDKind)
	if err != nil {
		return []VarDecl{}, err
	}

	for parser.currentToken().Kind == commaKind {
		err := parser.eatOnlyKind(commaKind)
		if err != nil {
			return []VarDecl{}, err
		}
		varNodes = append(varNodes, NewVar(parser.currentToken()))
		err = parser.eatOnlyKind(IDKind)
		if err != nil {
			return []VarDecl{}, err
		}
	}

	err = parser.eatOnlyKind(colonKind)
	if err != nil {
		return []VarDecl{}, err
	}

	typeNode, err := parser.typeSpec()
	if err != nil {
		return []VarDecl{}, err
	}

	var result []VarDecl
	for _, varNode := range varNodes {
		result = append(result, VarDecl{varNode, typeNode})
	}

	return result, nil
}

func (parser *Parser) typeSpec() (Type, error) {
	token := parser.currentToken()
	if token.Kind == keywordKind && token.Value == "INTEGER" {
		err := parser.eat(keywordKind, "INTEGER")
		if err != nil {
			return Type{}, err
		}
	} else if token.Kind == keywordKind && token.Value == "REAL" {
		err := parser.eat(keywordKind, "REAL")
		if err != nil {
			return Type{}, err
		}
	} else {
		return Type{}, errors.New(fmt.Sprintf("Error checking type, tokens %d: %v.", parser.cur, token))
	}

	return Type{token, token.NumericValue}, nil
}

func (parser *Parser) parse() (AST, error) {
	return parser.program()
}

/////// INTERPRETER
type Interpreter struct {
	node        AST
	globalScope map[string]int
}

func (itpr *Interpreter) visitBinOp(node AST) (int, error) {
	nodeBinOp := node.(BinOp)

	left, err := itpr.visit(nodeBinOp.Left)
	if err != nil {
		return -1, err
	}
	right, err := itpr.visit(nodeBinOp.Right)
	if err != nil {
		return -1, err
	}

	switch nodeBinOp.Op.Value {
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

func (itpr *Interpreter) visitNum(node AST) (int, error) {
	return node.(Num).Value.(int), nil
}

func (itpr *Interpreter) visitUnaryOp(node AST) (int, error) {
	n, ok := node.(UnaryOp)
	if !ok {
		return -1, errors.New("Error Unary Op")
	}
	switch n.Op.Value {
	case "PLUS":
		return itpr.visit(n.expr)
	case "MINUS":
		v, err := itpr.visit(n.expr)
		if err != nil {
			return -1, err
		}
		return -v, err
	}
	return -1, errors.New("Error visiting Unary Op")
}

func (itpr *Interpreter) visitCompound(node AST) (int, error) {
	for _, child := range node.(Compound).Children {
		_, err := itpr.visit(child)
		if err != nil {
			return -1, err
		}
	}

	return -1, nil
}

func (itpr *Interpreter) visitNoOp(node AST) (int, error) {
	return -1, nil
}

func (itpr *Interpreter) visitAssign(node AST) (int, error) {
	varName := node.(Assign).Left.(Var).Value
	v, err := itpr.visit(node.(Assign).Right)
	itpr.globalScope[varName] = v
	return -1, err
}

func (itpr *Interpreter) visitVar(node AST) (int, error) {
	varName := node.(Var).Value
	val, ok := itpr.globalScope[varName]
	if !ok {
		return -1, errors.New(fmt.Sprintf("No %v registered\n", varName))
	}
	return val, nil
}

func (itpr *Interpreter) visitProgram(node AST) error {
	_, err := itpr.visit(node.(Program).Block)
	return err
}

func (itpr *Interpreter) visitBlock(node AST) error {
	for _, dec := range node.(Block).Declarations {
		_, err := itpr.visit(dec)
		if err != nil {
			return err
		}
	}
	_, err := itpr.visit(node.(Block).CompoundStatement)
	return err
}

func (itpr *Interpreter) visitVarDecl(node AST) error {
	return nil
}

func (itpr *Interpreter) visitType(node AST) error {
	return nil
}

func (itpr *Interpreter) visit(node AST) (int, error) {
	switch node.(type) {
	case BinOp:
		return itpr.visitBinOp(node)
	case Num:
		return itpr.visitNum(node)
	case UnaryOp:
		return itpr.visitUnaryOp(node)
	case NoOp:
		return itpr.visitNoOp(node)
	case Compound:
		return itpr.visitCompound(node)
	case Var:
		return itpr.visitVar(node)
	case Assign:
		return itpr.visitAssign(node)
	default:
		return -1, errors.New("Unknown node type")
	}
}

func (itpr Interpreter) interprete() (int, error) {
	return itpr.visit(itpr.node)
}

///// ALL TOGETHER
func do(text string) (interface{}, error) {
	// 1. lexing: decompose string into tokens
	// also convert tokens into values based on their kinds
	tokens, err := lex(text)
	if err != nil {
		return -1, err
	}
	fmt.Println(tokens)

	// 2. parser: build AST representation
	parser := NewParser(tokens)
	node, err := parser.parse()
	if err != nil {
		return -1, err
	}
	return node, nil

	// // 3. interpreter: generate result
	// itpr := Interpreter{node: node, globalScope: make(map[string]int)}
	// itpr.interprete()
	// return itpr.globalScope, nil
}

func main() {
	// v, err := do("BEGIN a := 2; END.")
	v, err := do(`PROGRAM Part10AST;
VAR
   a, b : INTEGER;
   y    : REAL;

BEGIN {Part10AST}
   a := 2;
   b := 10 * a + 10 * a DIV 4;
   y := 20 / 7 + 3.14;
END. {Part10AST}`)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Printf("%+v\n", v)
}
