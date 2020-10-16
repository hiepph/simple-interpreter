package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"reflect"
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

var keywordList = []string{"BEGIN", "END", "PROCEDURE", "PROGRAM",
	"VAR", "DIV", "INTEGER", "REAL"}

type Token struct {
	Kind         tokenKind
	Value        string
	NumericValue interface{}
}

type AST interface {
}

func draw(node AST, indicator string) {
	var (
		emptySpace = "    "
		middleItem = "├── "
		// continueItem = "│  "
		// lastItem     = "└──"
	)

	fmt.Printf(indicator + middleItem)
	indicator += emptySpace
	var children []AST
	switch node.(type) {
	case Program:
		fmt.Printf("%T\n", node)
		children = []AST{node.(Program).Block}
	case Block:
		fmt.Printf("%T\n", node)
		for _, dec := range node.(Block).Declarations {
			children = append(children, dec)
		}
		children = append(children, node.(Block).CompoundStatement)
	case ProcedureDecl:
		fmt.Printf("%T: %v\n", node, node.(ProcedureDecl).Name)
		for _, param := range node.(ProcedureDecl).Params {
			children = append(children, param)
		}
		children = append(children, node.(ProcedureDecl).Block)
	case Compound:
		fmt.Printf("%T\n", node)
		for _, child := range node.(Compound).Children {
			children = append(children, child)
		}
	case Param:
		fmt.Printf("%T\n", node)
		children = []AST{node.(Param).VarNode,
			node.(Param).TypeNode}
	case VarDecl:
		fmt.Printf("%T\n", node)
		children = []AST{node.(VarDecl).VarNode,
			node.(VarDecl).TypeNode}
	case Var:
		fmt.Printf("%s\n", node.(Var).Value)
	case Type:
		fmt.Printf("%s\n", node.(Type).Token.Value)
	case Assign:
		fmt.Printf("%v\n", node.(Assign).Op.Value)
		children = append(children, node.(Assign).Left)
		children = append(children, node.(Assign).Right)
	case BinOp:
		fmt.Printf("%v\n", node.(BinOp).Op.Value)
		children = append(children, node.(BinOp).Left)
		children = append(children, node.(BinOp).Right)
	case Num:
		fmt.Printf("%v\n", node.(Num).Token.NumericValue)
	default:
		fmt.Printf("%T\n", node)
	}
	for _, child := range children {
		draw(child, indicator)
	}
}

type Program struct {
	Name  string
	Block Block
}

type Block struct {
	Declarations      []Decl
	CompoundStatement Compound
}

type Decl interface {
}

type VarDecl struct {
	VarNode  Var
	TypeNode Type
}

type ProcedureDecl struct {
	Name   string
	Block  Block
	Params []Param
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

type Symbol struct {
	Name   string
	Type   interface{}
	Params []interface{}
}

type Param struct {
	VarNode  Var
	TypeNode Type
}

func NewBuiltinTypeSymbol(name string) Symbol {
	var noParams []interface{}
	return Symbol{name, "BUILT-IN", noParams}
}

func NewVarSymbol(name string, typ Symbol) Symbol {
	var noParams []interface{}
	return Symbol{name, typ, noParams}
}

func NewProcedureSymbol(name string) Symbol {
	var noParams []interface{}
	return Symbol{name, "Procedure", noParams}
}

func (s *Symbol) addParam(param Symbol) {
	s.Params = append(s.Params, param)
}

var (
	intType  = NewBuiltinTypeSymbol("INTEGER")
	realType = NewBuiltinTypeSymbol("REAL")
)

type SymbolTable struct {
	Symbols        map[string]Symbol
	ScopeName      string
	ScopeLevel     int
	EnclosingScope interface{}
}

func NewSymbolTable(name string, level int) SymbolTable {
	t := SymbolTable{make(map[string]Symbol), name, level, nil}
	t.define(intType)
	t.define(realType)
	return t
}

func (t SymbolTable) String() string {
	b, err := json.MarshalIndent(t, "", "  ")
	if err == nil {
		return string(b)
	}
	return ""
}

func (t *SymbolTable) define(symbol Symbol) {
	t.Symbols[symbol.Name] = symbol
}

func (t SymbolTable) lookup(name string) (Symbol, bool) {
	s, ok := t.Symbols[name]
	return s, ok
}

type SemanticAnalyzer struct {
	Table SymbolTable
}

func NewSemanticAnalyzer() SemanticAnalyzer {
	return SemanticAnalyzer{}
}

func (sa *SemanticAnalyzer) visit(node AST) error {
	switch node.(type) {
	case Program:
		return sa.visitProgram(node)
	case Block:
		return sa.visitBlock(node)
	case BinOp:
		return sa.visitBinOp(node)
	case Num:
		return sa.visitNum(node)
	case UnaryOp:
		return sa.visitUnaryOp(node)
	case Compound:
		return sa.visitCompound(node)
	case NoOp:
		return sa.visitNoOp(node)
	case VarDecl:
		return sa.visitVarDecl(node)
	case ProcedureDecl:
		return sa.visitProcedureDecl(node)
	case Assign:
		return sa.visitAssign(node)
	case Var:
		return sa.visitVar(node)
	default:
		return errors.New(
			fmt.Sprintf("(SemanticAnalyzer) Unknown node type %T", node))
	}
}

func (sa *SemanticAnalyzer) visitBlock(node AST) error {
	for _, dec := range node.(Block).Declarations {
		err := sa.visit(dec)
		if err != nil {
			return err
		}
	}
	return sa.visit(node.(Block).CompoundStatement)
}

func (sa *SemanticAnalyzer) visitProgram(node AST) error {
	globalScope := NewSymbolTable("global", 1)
	sa.Table = globalScope

	return sa.visit(node.(Program).Block)
}

func (sa *SemanticAnalyzer) visitBinOp(node AST) error {
	nodeBinOp := node.(BinOp)

	err := sa.visit(nodeBinOp.Left)
	if err != nil {
		return err
	}
	err = sa.visit(nodeBinOp.Right)
	if err != nil {
		return err
	}

	return nil
}

func (sa *SemanticAnalyzer) visitNum(node AST) error {
	return nil
}

func (sa *SemanticAnalyzer) visitUnaryOp(node AST) error {
	return sa.visit(node.(UnaryOp).expr)
}

func (sa *SemanticAnalyzer) visitCompound(node AST) error {
	for _, child := range node.(Compound).Children {
		err := sa.visit(child)
		if err != nil {
			return err
		}
	}

	return nil
}

func (sa *SemanticAnalyzer) visitNoOp(node AST) error {
	return nil
}

func (sa *SemanticAnalyzer) visitVarDecl(node AST) error {
	typeName := node.(VarDecl).TypeNode.Token.Value
	typeSymbol, ok := sa.Table.lookup(typeName)
	if !ok {
		return errors.New(fmt.Sprintf("(VarDecl) Can't not find key %s\n", typeName))
	}
	varName := node.(VarDecl).VarNode.Token.Value
	varSymbol := NewVarSymbol(varName, typeSymbol)
	_, ok = sa.Table.lookup(varName)
	if ok {
		return errors.New(fmt.Sprintf("(VarDecl) Duplicate identifier %s\n", varName))
	}
	sa.Table.define(varSymbol)
	return nil
}

func (sa *SemanticAnalyzer) visitProcedureDecl(node AST) error {
	procName := node.(ProcedureDecl).Name
	procSymbol := NewProcedureSymbol(procName)
	sa.Table.define(procSymbol)

	procedureScope := NewSymbolTable(procName, sa.Table.ScopeLevel+1)
	procedureScope.EnclosingScope = sa.Table

	log.Printf("ENTER scope: %s\n", procName)
	// jump into child table
	sa.Table = procedureScope

	for _, param := range node.(ProcedureDecl).Params {
		typeName := param.TypeNode.Token.Value
		paramType, ok := sa.Table.lookup(typeName)
		if !ok {
			return errors.New(fmt.Sprintf("(ProcedureDecl) Can't not find key %s\n", typeName))
		}
		paramName := param.VarNode.Token.Value
		varSymbol := NewVarSymbol(paramName, paramType)
		sa.Table.define(varSymbol)
		procSymbol.addParam(varSymbol)
	}

	err := sa.visit(node.(ProcedureDecl).Block)
	if err != nil {
		return err
	}
	log.Println(sa.Table)

	// jump back into parent table
	sa.Table = procedureScope.EnclosingScope.(SymbolTable)

	log.Printf("LEAVE scope: %s\n", procName)
	return nil
}

func (sa *SemanticAnalyzer) visitAssign(node AST) error {
	varName := node.(Assign).Left.(Var).Value
	_, ok := sa.Table.lookup(varName)
	if !ok {
		return errors.New(fmt.Sprintf("(Assign) Can't not find key %s\n", varName))
	}
	return sa.visit(node.(Assign).Right)
}

func (sa *SemanticAnalyzer) visitVar(node AST) error {
	varName := node.(Var).Value
	_, ok := sa.Table.lookup(varName)
	if !ok {
		return errors.New(fmt.Sprintf("(Var) Can't not find key %s\n", varName))
	}

	return nil
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
		return nil, err
	}
	compoundStatementNode, err := parser.compoundStatement()
	if err != nil {
		return nil, err
	}
	return Block{declarationNodes, compoundStatementNode.(Compound)}, nil
}

func (parser *Parser) declarations() ([]Decl, error) {
	// declarations: (VAR (variable_declaration SEMI)+)*
	//             | (PROCEDURE ID (LPAREN formal_parameter_list RPAREN)? SEMI block SEMI)*
	//             | empty
	var result []Decl
	for {
		if parser.currentToken().Kind == keywordKind &&
			parser.currentToken().Value == "VAR" {
			err := parser.eat(keywordKind, "VAR")
			if err != nil {
				return []Decl{}, err
			}
			for parser.currentToken().Kind == IDKind {
				varDecls, err := parser.variableDeclarations()
				if err != nil {
					return []Decl{}, err
				}
				for _, varDecl := range varDecls {
					result = append(result, varDecl)
				}
				err = parser.eatOnlyKind(semiKind)
				if err != nil {
					return []Decl{}, err
				}
			}
		} else if parser.currentToken().Kind == keywordKind &&
			parser.currentToken().Value == "PROCEDURE" {
			err := parser.eat(keywordKind, "PROCEDURE")
			if err != nil {
				return []Decl{}, err
			}

			procName := parser.currentToken().Value

			err = parser.eatOnlyKind(IDKind)
			if err != nil {
				return []Decl{}, err
			}

			var params []Param
			if parser.currentToken().Kind == operatorKind &&
				parser.currentToken().Value == "LPAREN" {
				parser.eat(operatorKind, "LPAREN")

				params, err = parser.formalParameterList()
				if err != nil {
					return []Decl{}, err
				}

				err = parser.eat(operatorKind, "RPAREN")
				if err != nil {
					return []Decl{}, err
				}
			}

			err = parser.eatOnlyKind(semiKind)
			if err != nil {
				return []Decl{}, err
			}

			blockNode, err := parser.block()
			if err != nil {
				return []Decl{}, err
			}

			procDecl := ProcedureDecl{procName, blockNode.(Block), params}
			result = append(result, procDecl)

			err = parser.eatOnlyKind(semiKind)
			if err != nil {
				return []Decl{}, err
			}
		} else {
			break
		}
	}

	return result, nil
}

func (parser *Parser) formalParameterList() ([]Param, error) {
	// formal_parameter_list: formal_parameters
	//                      | formal_parameters SEMI formal_parameter_list
	if parser.currentToken().Kind != IDKind {
		return []Param{}, nil
	}

	paramNodes, err := parser.formalParameters()
	if err != nil {
		return []Param{}, err
	}

	for parser.currentToken().Kind == semiKind {
		parser.eatOnlyKind(semiKind)
		params, err := parser.formalParameters()
		if err != nil {
			return []Param{}, err
		}
		paramNodes = append(paramNodes, params...)
	}

	return paramNodes, nil
}

func (parser *Parser) formalParameters() ([]Param, error) {
	// formal_parameters: ID (COMMA ID)* COLON type_spec
	var paramNodes []Param

	paramTokens := []Token{parser.currentToken()}
	err := parser.eatOnlyKind(IDKind)
	if err != nil {
		return []Param{}, err
	}

	for parser.currentToken().Kind == commaKind {
		parser.eatOnlyKind(commaKind)
		paramTokens = append(paramTokens, parser.currentToken())
		err = parser.eatOnlyKind(IDKind)
		if err != nil {
			return []Param{}, err
		}
	}

	err = parser.eatOnlyKind(colonKind)
	typeNode, err := parser.typeSpec()
	if err != nil {
		return []Param{}, err
	}

	for _, paramToken := range paramTokens {
		paramNodes = append(paramNodes, Param{NewVar(paramToken), typeNode})
	}

	return paramNodes, nil
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
		result = append(result, VarDecl{varNode.(Var), typeNode})
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
	globalScope map[string]interface{}
}

func (itpr *Interpreter) visisainOp(node AST) (interface{}, error) {
	nodeBinOp := node.(BinOp)

	left, err := itpr.visit(nodeBinOp.Left)
	if err != nil {
		return nil, err
	}
	right, err := itpr.visit(nodeBinOp.Right)
	if err != nil {
		return nil, err
	}

	switch nodeBinOp.Op.Value {
	case "PLUS":
		if reflect.TypeOf(left).Kind() == reflect.Int &&
			reflect.TypeOf(right).Kind() == reflect.Int {
			return left.(int) + right.(int), nil
		}
		if reflect.TypeOf(left).Kind() == reflect.Int &&
			reflect.TypeOf(right).Kind() == reflect.Float64 {
			return float64(left.(int)) + right.(float64), nil
		}
		if reflect.TypeOf(left).Kind() == reflect.Float64 &&
			reflect.TypeOf(right).Kind() == reflect.Int {
			return left.(float64) + float64(right.(int)), nil
		}
		return left.(float64) + right.(float64), nil
	case "MINUS":
		if reflect.TypeOf(left).Kind() == reflect.Int &&
			reflect.TypeOf(right).Kind() == reflect.Int {
			return left.(int) - right.(int), nil
		}
		if reflect.TypeOf(left).Kind() == reflect.Int &&
			reflect.TypeOf(right).Kind() == reflect.Float64 {
			return float64(left.(int)) - right.(float64), nil
		}
		if reflect.TypeOf(left).Kind() == reflect.Float64 &&
			reflect.TypeOf(right).Kind() == reflect.Int {
			return left.(float64) - float64(right.(int)), nil
		}
		return left.(float64) - right.(float64), nil
	case "MUL":
		if reflect.TypeOf(left).Kind() == reflect.Int &&
			reflect.TypeOf(right).Kind() == reflect.Int {
			return left.(int) * right.(int), nil
		}
		if reflect.TypeOf(left).Kind() == reflect.Int &&
			reflect.TypeOf(right).Kind() == reflect.Float64 {
			return float64(left.(int)) * right.(float64), nil
		}
		if reflect.TypeOf(left).Kind() == reflect.Float64 &&
			reflect.TypeOf(right).Kind() == reflect.Int {
			return left.(float64) * float64(right.(int)), nil
		}
		return left.(float64) * right.(float64), nil
	case "DIV":
		if reflect.TypeOf(left).Kind() == reflect.Int &&
			reflect.TypeOf(right).Kind() == reflect.Int {
			return left.(int) / right.(int), nil
		} else {
			return nil, errors.New("Cannot DIV for float values")
		}
	case "FLOAT_DIV":
		if reflect.TypeOf(left).Kind() == reflect.Int &&
			reflect.TypeOf(right).Kind() == reflect.Int {
			return float64(left.(int)) / float64(right.(int)), nil
		}
		if reflect.TypeOf(left).Kind() == reflect.Int &&
			reflect.TypeOf(right).Kind() == reflect.Float64 {
			return float64(left.(int)) / right.(float64), nil
		}
		if reflect.TypeOf(left).Kind() == reflect.Float64 &&
			reflect.TypeOf(right).Kind() == reflect.Int {
			return left.(float64) / float64(right.(int)), nil
		}
		return left.(float64) / right.(float64), nil
	default:
		return nil, errors.New("Unknown Op")
	}
}

func (itpr *Interpreter) visitNum(node AST) (interface{}, error) {
	return node.(Num).Value, nil
}

func (itpr *Interpreter) visitUnaryOp(node AST) (interface{}, error) {
	n, ok := node.(UnaryOp)
	if !ok {
		return nil, errors.New("Error Unary Op")
	}
	switch n.Op.Value {
	case "PLUS":
		return itpr.visit(n.expr)
	case "MINUS":
		v, err := itpr.visit(n.expr)
		if err != nil {
			return nil, err
		}
		decimalV, ok := v.(int)
		if ok {
			return -decimalV, err
		} else {
			return -v.(float64), err
		}
	}
	return nil, errors.New("Error visiting Unary Op")
}

func (itpr *Interpreter) visitCompound(node AST) (interface{}, error) {
	for _, child := range node.(Compound).Children {
		_, err := itpr.visit(child)
		if err != nil {
			return nil, err
		}
	}

	return nil, nil
}

func (itpr *Interpreter) visitNoOp(node AST) (interface{}, error) {
	return nil, nil
}

func (itpr *Interpreter) visitAssign(node AST) (interface{}, error) {
	varName := node.(Assign).Left.(Var).Value
	v, err := itpr.visit(node.(Assign).Right)
	itpr.globalScope[varName] = v
	return nil, err
}

func (itpr *Interpreter) visitVar(node AST) (interface{}, error) {
	varName := node.(Var).Value
	val, ok := itpr.globalScope[varName]
	if !ok {
		return nil, errors.New(fmt.Sprintf("No %v registered\n", varName))
	}
	return val, nil
}

func (itpr *Interpreter) visitProgram(node AST) (interface{}, error) {
	return itpr.visit(node.(Program).Block)
}

func (itpr *Interpreter) visisalock(node AST) (interface{}, error) {
	for _, dec := range node.(Block).Declarations {
		_, err := itpr.visit(dec)
		if err != nil {
			return nil, err
		}
	}
	return itpr.visit(node.(Block).CompoundStatement)
}

func (itpr *Interpreter) visitVarDecl(node AST) (interface{}, error) {
	return nil, nil
}

func (itpr *Interpreter) visitProcedureDecl(node AST) (interface{}, error) {
	return nil, nil
}

func (itpr *Interpreter) visitType(node AST) (interface{}, error) {
	return nil, nil
}

func (itpr *Interpreter) visit(node AST) (interface{}, error) {
	switch node.(type) {
	case BinOp:
		return itpr.visisainOp(node)
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
	case Program:
		return itpr.visitProgram(node)
	case Block:
		return itpr.visisalock(node)
	case VarDecl:
		return itpr.visitVarDecl(node)
	case ProcedureDecl:
		return itpr.visitProcedureDecl(node)
	case Type:
		return itpr.visitType(node)
	default:
		return nil, errors.New(
			fmt.Sprintf("(interpreter) Unknown node type %T", node))
	}
}

func (itpr Interpreter) interprete() (interface{}, error) {
	return itpr.visit(itpr.node)
}

///// ALL TOGETHER
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
	fmt.Println()

	// 3. interpreter: generate result
	semanticAnalyzer := NewSemanticAnalyzer()
	err = semanticAnalyzer.visit(node)
	if err != nil {
		return nil, err
	}
	fmt.Println("------ SymbolTable --------")
	fmt.Println(semanticAnalyzer.Table)

	// itpr := Interpreter{node: node, globalScope: make(map[string]interface{})}
	// _, err = itpr.interprete()
	// if err != nil {
	// 	return nil, err
	// }
	// fmt.Println("-------- globalScope -------")
	// fmt.Println(itpr.globalScope)

	return nil, nil
}

func main() {
	content, err := ioutil.ReadFile(os.Args[1])
	if err != nil {
		log.Fatal(err)
	}
	// fmt.Println(string(content))
	_, err = do(string(content))

	if err != nil {
		log.Fatal(err)
	}
}
