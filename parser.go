package main

import (
	"errors"
	"fmt"
)

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

type Param struct {
	VarNode  Var
	TypeNode Type
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
	// factor : + factor
	//        | - factor
	//        | INTEGER_CONST
	//        | REAL_CONST
	//        | ( expr )
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
		if token.Value == "(" {
			err = parser.eat(operatorKind, "(")
			if err != nil {
				return nil, err
			}
			node, err := parser.expr()
			if err != nil {
				return nil, err
			}
			err = parser.eat(operatorKind, ")")
			if err != nil {
				return nil, err
			}
			return node, nil
		} else if token.Value == "+" || token.Value == "-" {
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
	// term : factor ((* | DIV | /) factor)*
	var err error

	node, err := parser.factor()
	if err != nil {
		return nil, err
	}

	for parser.currentToken().Kind == operatorKind &&
		contains([]string{"*", "DIV", "/"}, parser.currentToken().Value) {
		token := parser.currentToken()
		switch token.Value {
		case "*":
			err = parser.eat(operatorKind, "*")
		case "DIV":
			err = parser.eat(operatorKind, "DIV")
		case "/":
			err = parser.eat(operatorKind, "/")
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
	// expr: term ((*|DIV)term)*
	// term: factor ((*|DIV)factor)*
	// factor: (+|-) factor | INTEGER | ( expr )
	node, err := parser.term()
	if err != nil {
		return nil, err
	}

	for parser.currentToken().Kind == operatorKind &&
		contains([]string{"+", "-"}, parser.currentToken().Value) {
		token := parser.currentToken()
		switch token.Value {
		case "+":
			err := parser.eat(operatorKind, "+")
			if err != nil {
				return nil, err
			}
		case "-":
			err := parser.eat(operatorKind, "-")
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
	//             | (PROCEDURE ID (( formal_parameter_list ))? SEMI block SEMI)*
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
				parser.currentToken().Value == "(" {
				parser.eat(operatorKind, "(")

				params, err = parser.formalParameterList()
				if err != nil {
					return []Decl{}, err
				}

				err = parser.eat(operatorKind, ")")
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
