package main

import (
	"errors"
	"fmt"
	"log"
)

type Symbol struct {
	Name   string
	Type   interface{}
	Params []interface{}
	Scope  SymbolTable
}

func NewBuiltinTypeSymbol(name string) Symbol {
	var noParams []interface{}
	return Symbol{name, "BUILT-IN", noParams, SymbolTable{}}
}

func NewVarSymbol(name string, typ Symbol) Symbol {
	var noParams []interface{}
	return Symbol{name, typ, noParams, SymbolTable{}}
}

func NewProcedureSymbol(name string) Symbol {
	var noParams []interface{}
	return Symbol{name, "Procedure", noParams, SymbolTable{}}
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
	// t.insert(intType)
	// t.insert(realType)
	return t
}

// func (t SymbolTable) String() string {
// 	b, err := json.MarshalIndent(t, "", "  ")
// 	if err == nil {
// 		return string(b)
// 	}
// 	return ""
// }

func (t *SymbolTable) insert(symbol Symbol) {
	// store scope for accessing variable's scope level
	symbol.Scope = *t
	// log.Printf("INSERT: %s\n", symbol.Name)
	t.Symbols[symbol.Name] = symbol
}

func (t SymbolTable) lookup(name string) (Symbol, bool) {
	// log.Printf("LOOKUP: %s (scope name: %s)\n",
	// 	name, t.ScopeName)
	s, ok := t.Symbols[name]
	if ok {
		return s, ok
	}

	if t.EnclosingScope == nil {
		return Symbol{}, false
	}
	// recursively search parent table
	return t.EnclosingScope.(SymbolTable).lookup(name)
}

func (t SymbolTable) lookupCurrentScope(name string) (Symbol, bool) {
	s, ok := t.Symbols[name]
	if ok {
		return s, ok
	}

	return Symbol{}, false
}

type SemanticAnalyzer struct {
	Table SymbolTable
}

func NewSemanticAnalyzer() SemanticAnalyzer {
	return SemanticAnalyzer{}
}

func (sa *SemanticAnalyzer) visit(node AST) (AST, error) {
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
	case ProcedureCall:
		return sa.visitProcedureCall(node)
	default:
		return nil, errors.New(
			fmt.Sprintf("(SemanticAnalyzer) Unknown node type %T", node))
	}
}

func (sa *SemanticAnalyzer) visitBlock(node AST) (AST, error) {
	var newDeclarations []Decl
	for _, dec := range node.(Block).Declarations {
		newNode, err := sa.visit(dec)
		newDeclarations = append(newDeclarations, newNode.(Decl))
		if err != nil {
			return nil, err
		}
	}
	var newBlock = node.(Block)
	newBlock.Declarations = newDeclarations

	newCompound, err := sa.visit(newBlock.CompoundStatement)
	newBlock.CompoundStatement = newCompound.(Compound)
	return newBlock, err
}

func (sa *SemanticAnalyzer) visitProgram(node AST) (AST, error) {
	log.Println("ENTER scope: global")
	globalScope := NewSymbolTable("global", 1)
	globalScope.insert(intType)
	globalScope.insert(realType)
	sa.Table = globalScope

	blockNode, err := sa.visit(node.(Program).Block)
	var newProgram = node.(Program)
	newProgram.Block = blockNode.(Block)
	if err != nil {
		return nil, err
	}

	log.Println("LEAVE scope: global")
	return newProgram, nil
}

func (sa *SemanticAnalyzer) visitBinOp(node AST) (AST, error) {
	nodeBinOp := node.(BinOp)

	_, err := sa.visit(nodeBinOp.Left)
	if err != nil {
		return nil, err
	}
	_, err = sa.visit(nodeBinOp.Right)
	if err != nil {
		return nil, err
	}

	return node, nil
}

func (sa *SemanticAnalyzer) visitNum(node AST) (AST, error) {
	return node, nil
}

func (sa *SemanticAnalyzer) visitUnaryOp(node AST) (AST, error) {
	return sa.visit(node.(UnaryOp).expr)
}

func (sa *SemanticAnalyzer) visitCompound(node AST) (AST, error) {
	var newChildren []AST
	for _, child := range node.(Compound).Children {
		newNode, err := sa.visit(child)
		newChildren = append(newChildren, newNode)
		switch child.(type) {
		case ProcedureCall:
		}
		if err != nil {
			return nil, err
		}
	}

	var newCompound = node.(Compound)
	newCompound.Children = newChildren

	return newCompound, nil
}

func (sa *SemanticAnalyzer) visitNoOp(node AST) (AST, error) {
	return node, nil
}

func (sa *SemanticAnalyzer) visitVarDecl(node AST) (AST, error) {
	typeName := node.(VarDecl).TypeNode.Token.Value
	typeSymbol, _ := sa.Table.lookup(typeName)

	varName := node.(VarDecl).VarNode.Token.Value
	varSymbol := NewVarSymbol(varName, typeSymbol)
	_, ok := sa.Table.lookupCurrentScope(varName)
	if ok {
		return nil, &SemanticError{DuplicateID,
			node.(VarDecl).VarNode.Token}
	}
	sa.Table.insert(varSymbol)
	return node, nil
}

func (sa *SemanticAnalyzer) visitProcedureDecl(node AST) (AST, error) {
	procName := node.(ProcedureDecl).Name
	procSymbol := NewProcedureSymbol(procName)
	sa.Table.insert(procSymbol)

	log.Printf("ENTER scope: %s\n", procName)
	procedureScope := NewSymbolTable(procName, sa.Table.ScopeLevel+1)
	procedureScope.EnclosingScope = sa.Table

	// jump into child table
	sa.Table = procedureScope

	for _, param := range node.(ProcedureDecl).Params {
		typeName := param.TypeNode.Token.Value
		paramType, ok := sa.Table.lookup(typeName)
		if !ok {
			return nil, errors.New(fmt.Sprintf("(ProcedureDecl) Can't not find key %s\n", typeName))
		}
		paramName := param.VarNode.Token.Value
		varSymbol := NewVarSymbol(paramName, paramType)
		sa.Table.insert(varSymbol)
		procSymbol.addParam(varSymbol)
	}

	_, err := sa.visit(node.(ProcedureDecl).Block)
	if err != nil {
		return nil, err
	}

	// jump back into parent table
	sa.Table = procedureScope.EnclosingScope.(SymbolTable)

	log.Printf("LEAVE scope: %s\n", procName)
	return node, nil
}

func (sa *SemanticAnalyzer) visitProcedureCall(node AST) (AST, error) {
	for _, paramNode := range node.(ProcedureCall).Params {
		_, err := sa.visit(paramNode)
		if err != nil {
			return nil, err
		}
	}
	procName := node.(ProcedureCall).Name
	procSymbol, ok := sa.Table.lookup(procName)
	if !ok {
		return nil, errors.New(fmt.Sprintf(
			"(ProcedureCall) Can't find key %s", procName))
	}
	var newProc = node.(ProcedureCall)
	newProc.Symbol = procSymbol

	return newProc, nil
}

func (sa *SemanticAnalyzer) visitAssign(node AST) (AST, error) {
	varName := node.(Assign).Left.(Var).Value
	_, ok := sa.Table.lookup(varName)
	if !ok {
		return nil, errors.New(fmt.Sprintf("(Assign) Can't find key %s\n", varName))
	}
	return sa.visit(node.(Assign).Right)
}

func (sa *SemanticAnalyzer) visitVar(node AST) (AST, error) {
	varName := node.(Var).Value
	_, ok := sa.Table.lookup(varName)
	if !ok {
		return nil, &SemanticError{IdNotFound, node.(Var).Token}
	}

	return node, nil
}
