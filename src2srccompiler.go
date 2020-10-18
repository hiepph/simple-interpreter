package main

import (
	"errors"
	"fmt"
	"strings"
)

const (
	INDENT = "    "
)

type SourceToSourceCompiler struct {
	Table SymbolTable
}

func NewSourceToSourceCompiler() SourceToSourceCompiler {
	return SourceToSourceCompiler{}
}

func (c *SourceToSourceCompiler) visit(node AST) (string, error) {
	switch node.(type) {
	case Program:
		return c.visitProgram(node)
	case Block:
		return c.visitBlock(node)
	case BinOp:
		return c.visitBinOp(node)
	case Compound:
		return c.visitCompound(node)
	case NoOp:
		return c.visitNoOp(node)
	case VarDecl:
		return c.visitVarDecl(node)
	case ProcedureDecl:
		return c.visitProcedureDecl(node)
	case Assign:
		return c.visitAssign(node)
	case Var:
		return c.visitVar(node)
	default:
		return "", errors.New(
			fmt.Sprintf("(SourceToSourceCompiler) Unknown node type %T", node))
	}
}

func (c *SourceToSourceCompiler) visitProgram(node AST) (string, error) {
	progName := node.(Program).Name
	res := fmt.Sprintf("PROGRAM %s0;\n", progName)

	globalScope := NewSymbolTable("global", 1)
	globalScope.insert(intType)
	globalScope.insert(realType)
	c.Table = globalScope

	blockStr, err := c.visit(node.(Program).Block)
	if err != nil {
		return "", err
	}
	res += fmt.Sprintf("%s. {END OF %s}", blockStr, progName)

	return res, nil
}

func (c *SourceToSourceCompiler) visitBlock(node AST) (string, error) {
	result := []string{}
	for _, dec := range node.(Block).Declarations {
		s, err := c.visit(dec)
		if err != nil {
			return "", err
		}
		result = append(result, s)
	}

	result = append(result, "\nBEGIN")
	s, err := c.visit(node.(Block).CompoundStatement)
	if err != nil {
		return "", err
	}
	result = append(result, INDENT+s)
	result = append(result, "END")
	return strings.Join(result, "\n"), nil
}

func (c *SourceToSourceCompiler) visitVarDecl(node AST) (string, error) {
	typeName := node.(VarDecl).TypeNode.Token.Value
	typeSymbol, ok := c.Table.lookup(typeName)
	if !ok {
		return "", errors.New(fmt.Sprintf("(VarDecl) Can't not find key %s\n", typeName))
	}
	varName := node.(VarDecl).VarNode.Token.Value
	varSymbol := NewVarSymbol(varName, typeSymbol)

	// TODO: duplication

	c.Table.insert(varSymbol)

	return fmt.Sprintf("%sVAR %s%d: %s;", INDENT, varName, c.Table.ScopeLevel, typeName), nil
}

func (c *SourceToSourceCompiler) visitCompound(node AST) (string, error) {
	result := []string{}
	for _, child := range node.(Compound).Children {
		s, err := c.visit(child)
		if err != nil {
			return "", err
		}
		result = append(result, s)
	}

	return strings.Join(result, "\n"), nil
}

func (c *SourceToSourceCompiler) visitProcedureDecl(node AST) (string, error) {
	procName := node.(ProcedureDecl).Name
	procSymbol := NewProcedureSymbol(procName)
	c.Table.insert(procSymbol)

	result := []string{}

	procedureScope := NewSymbolTable(procName, c.Table.ScopeLevel+1)
	procedureScope.EnclosingScope = c.Table

	procedureStr := fmt.Sprintf("PROCEDURE %s%d", procName, c.Table.ScopeLevel)

	// jump into child table
	c.Table = procedureScope

	params := node.(ProcedureDecl).Params
	if len(params) > 0 {
		procedureStr += "("
		formalParams := []string{}
		for _, param := range params {
			typeName := param.TypeNode.Token.Value
			paramType, ok := c.Table.lookup(typeName)
			if !ok {
				return "", errors.New(
					fmt.Sprintf("(ProcedureDecl) Can't not find key %s\n", typeName))
			}
			paramName := param.VarNode.Token.Value
			varSymbol := NewVarSymbol(paramName, paramType)
			c.Table.insert(varSymbol)
			procSymbol.addParam(varSymbol)

			formalParams = append(formalParams,
				fmt.Sprintf("%s%d: %s", paramName, c.Table.ScopeLevel, paramType.Name))
		}
		procedureStr += strings.Join(formalParams, "; ")
		procedureStr += ")"
	}
	result = append(result, procedureStr+";")

	s, err := c.visit(node.(ProcedureDecl).Block)
	if err != nil {
		return "", err
	}
	result = append(result, s+fmt.Sprintf("; {END OF %s}", procName))

	// jump back into parent table
	c.Table = procedureScope.EnclosingScope.(SymbolTable)

	// indent procedure
	indentResult := []string{}
	for _, l := range strings.Split(strings.Join(result, "\n"), "\n") {
		indentResult = append(indentResult, INDENT+l)
	}
	return strings.Join(indentResult, "\n"), nil
}

func (c *SourceToSourceCompiler) visitNoOp(node AST) (string, error) {
	return "", nil
}

func (c *SourceToSourceCompiler) visitAssign(node AST) (string, error) {
	sr, err := c.visit(node.(Assign).Right)
	if err != nil {
		return "", err
	}
	sl, err := c.visit(node.(Assign).Left)
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("%s := %s;", sl, sr), nil
}

func (c *SourceToSourceCompiler) visitVar(node AST) (string, error) {
	varName := node.(Var).Value
	varSymbol, ok := c.Table.lookup(varName)
	if !ok {
		return "", errors.New(fmt.Sprintf("(Var) Can't not find key %s\n", varName))
	}

	return fmt.Sprintf("<%s%d:%s>", varName, varSymbol.Scope.ScopeLevel,
		varSymbol.Type.(Symbol).Name), nil
}

func (c *SourceToSourceCompiler) visitBinOp(node AST) (string, error) {
	sr, err := c.visit(node.(BinOp).Right)
	if err != nil {
		return "", err
	}
	sl, err := c.visit(node.(BinOp).Left)
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("%s %s %s", sl, node.(BinOp).Op.Value, sr), nil
}
