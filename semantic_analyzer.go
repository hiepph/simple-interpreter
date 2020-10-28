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
	log.Println("ENTER scope: global")
	globalScope := NewSymbolTable("global", 1)
	globalScope.insert(intType)
	globalScope.insert(realType)
	sa.Table = globalScope

	err := sa.visit(node.(Program).Block)
	if err != nil {
		return err
	}

	log.Println("LEAVE scope: global")
	return nil
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
	typeSymbol, _ := sa.Table.lookup(typeName)

	varName := node.(VarDecl).VarNode.Token.Value
	varSymbol := NewVarSymbol(varName, typeSymbol)
	_, ok := sa.Table.lookupCurrentScope(varName)
	if ok {
		return &SemanticError{DuplicateID, node.(VarDecl).VarNode.Token}
	}
	sa.Table.insert(varSymbol)
	return nil
}

func (sa *SemanticAnalyzer) visitProcedureDecl(node AST) error {
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
			return errors.New(fmt.Sprintf("(ProcedureDecl) Can't not find key %s\n", typeName))
		}
		paramName := param.VarNode.Token.Value
		varSymbol := NewVarSymbol(paramName, paramType)
		sa.Table.insert(varSymbol)
		procSymbol.addParam(varSymbol)
	}

	err := sa.visit(node.(ProcedureDecl).Block)
	if err != nil {
		return err
	}

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
		return &SemanticError{IdNotFound, node.(Var).Token}
	}

	return nil
}
