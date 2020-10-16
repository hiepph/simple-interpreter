package main

import (
	"errors"
	"fmt"
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
	// case Block:
	// 	return c.visitBlock(node)
	// case BinOp:
	// 	return c.visitBinOp(node)
	// case Num:
	// 	return c.visitNum(node)
	// case UnaryOp:
	// 	return c.visitUnaryOp(node)
	// case Compound:
	// 	return c.visitCompound(node)
	// case NoOp:
	// 	return c.visitNoOp(node)
	// case VarDecl:
	// 	return c.visitVarDecl(node)
	// case ProcedureDecl:
	// 	return c.visitProcedureDecl(node)
	// case Assign:
	// 	return c.visitAssign(node)
	// case Var:
	// 	return c.visitVar(node)
	default:
		return "", errors.New(
			fmt.Sprintf("(SourceToSourceCompiler) Unknown node type %T", node))
	}
}

func (c *SourceToSourceCompiler) visitProgram(node AST) (string, error) {
	progName := node.(Program).Name
	res := fmt.Sprintf("PROGRAM %s0:\n", progName)

	globalScope := NewSymbolTable("global", 1)
	globalScope.insert(intType)
	globalScope.insert(realType)
	c.Table = globalScope

	// blockStr, err := c.visit(node.(Program).Block)
	// if err != nil {
	// 	return "", err
	// }
	blockStr := "??"
	res += fmt.Sprintf("%s. {END OF %s}", blockStr, progName)

	return res, nil
}
