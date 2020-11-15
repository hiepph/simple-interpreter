package main

import (
	"errors"
	"fmt"
	"log"
)

type Symbol struct {
	Name       string
	Type       interface{}
	Params     []interface{}
	Scope      SymbolTable
	ScopeLevel int
	Block      AST
}

func NewBuiltinTypeSymbol(name string) Symbol {
	var noParams []interface{}
	return Symbol{name, "BUILT-IN", noParams, SymbolTable{}, 0, nil}
}

func NewVarSymbol(name string, typ Symbol) Symbol {
	var noParams []interface{}
	return Symbol{name, typ, noParams, SymbolTable{}, 0, nil}
}

func NewProcedureSymbol(name string) Symbol {
	var noParams []interface{}
	return Symbol{name, "Procedure", noParams, SymbolTable{}, 0, nil}
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
	symbol.ScopeLevel = t.ScopeLevel
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
		if err != nil {
			return nil, err
		}
		newDeclarations = append(newDeclarations, newNode.(Decl))
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

	leftNode, err := sa.visit(nodeBinOp.Left)
	if err != nil {
		return nil, err
	}
	rightNode, err := sa.visit(nodeBinOp.Right)
	if err != nil {
		return nil, err
	}

	var newBinOp = node.(BinOp)
	newBinOp.Left = leftNode
	newBinOp.Right = rightNode

	return newBinOp, nil
}

func (sa *SemanticAnalyzer) visitNum(node AST) (AST, error) {
	return node, nil
}

func (sa *SemanticAnalyzer) visitUnaryOp(node AST) (AST, error) {
	newExpr, err := sa.visit(node.(UnaryOp).expr)
	if err != nil {
		return nil, err
	}
	var newNode = node.(UnaryOp)
	newNode.expr = newExpr
	return newNode, nil
}

func (sa *SemanticAnalyzer) visitCompound(node AST) (AST, error) {
	var newChildren []AST
	for _, child := range node.(Compound).Children {
		newNode, err := sa.visit(child)
		if err != nil {
			return nil, err
		}
		newChildren = append(newChildren, newNode)
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
		// NOTE: need to replace this updated value inside the table
		procSymbol.addParam(varSymbol)
		// sa.Table.insert(procSymbol)
	}

	blockNode, err := sa.visit(node.(ProcedureDecl).Block)
	if err != nil {
		return nil, err
	}
	var newProcedureDecl = node.(ProcedureDecl)
	newProcedureDecl.Block = blockNode.(Block)

	// jump back into parent table
	sa.Table = procedureScope.EnclosingScope.(SymbolTable)

	// accessed by the interpreter when executing procedure call
	// NOTE: need to update the procSymbol followed new value of Block
	procSymbol.Block = newProcedureDecl.Block
	sa.Table.insert(procSymbol)
	log.Printf("LEAVE scope: %s\n", procName)

	return newProcedureDecl, nil
}

func (sa *SemanticAnalyzer) visitProcedureCall(node AST) (AST, error) {
	var newParams []AST
	for _, paramNode := range node.(ProcedureCall).Params {
		newParamNode, err := sa.visit(paramNode)
		newParams = append(newParams, newParamNode)
		if err != nil {
			return nil, err
		}
	}
	var newProc = node.(ProcedureCall)
	newProc.Params = newParams

	procName := node.(ProcedureCall).Name
	procSymbol, ok := sa.Table.lookup(procName)
	if !ok {
		return nil, errors.New(fmt.Sprintf(
			"(ProcedureCall) Can't find key %s", procName))
	}
	newProc.Symbol = procSymbol

	return newProc, nil
}

func (sa *SemanticAnalyzer) visitAssign(node AST) (AST, error) {
	varName := node.(Assign).Left.(Var).Value
	_, ok := sa.Table.lookup(varName)
	if !ok {
		return nil, errors.New(fmt.Sprintf("(Assign) Can't find key %s\n", varName))
	}

	var newNode = node.(Assign)
	rightNode, err := sa.visit(node.(Assign).Right)
	if err != nil {
		return nil, err
	}
	newNode.Right = rightNode
	return newNode, nil
}

func (sa *SemanticAnalyzer) visitVar(node AST) (AST, error) {
	varName := node.(Var).Value
	_, ok := sa.Table.lookup(varName)
	if !ok {
		return nil, &SemanticError{IdNotFound, node.(Var).Token}
	}

	return node, nil
}
