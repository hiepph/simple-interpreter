package main

import (
	"errors"
	"fmt"
	"log"
	"reflect"
)

type Stack struct {
	Items []ActivationRecord
}

func (s *Stack) push(item ActivationRecord) {
	s.Items = append(s.Items, item)
}

func (s *Stack) pop() ActivationRecord {
	var lastItem = s.Items[len(s.Items)-1]
	s.Items = s.Items[:len(s.Items)-1]
	return lastItem
}

func (s Stack) peek() ActivationRecord {
	return s.Items[len(s.Items)-1]
}

type ARType string

const (
	ProgramType ARType = "PROGRAM"
)

type ActivationRecord struct {
	Name         string
	Type         ARType
	NestingLevel int
	Members      map[string]interface{}
}

func (ar *ActivationRecord) set(k string, v interface{}) {
	ar.Members[k] = v
}

func (ar ActivationRecord) get(k string) interface{} {
	return ar.Members[k]
}

type Interpreter struct {
	node      AST
	CallStack Stack
}

func (itpr *Interpreter) visitBinOp(node AST) (interface{}, error) {
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
	case "+":
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
	case "-":
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
	case "*":
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
	case "/":
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
		return nil, errors.New(fmt.Sprintf("Unknown Op %s", nodeBinOp.Op.Value))
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
	ar := itpr.CallStack.peek()
	ar.set(varName, v)
	return nil, err
}

func (itpr *Interpreter) visitVar(node AST) (interface{}, error) {
	varName := node.(Var).Value
	ar := itpr.CallStack.peek()
	val := ar.get(varName)
	return val, nil
}

func (itpr *Interpreter) visitProgram(node AST) (interface{}, error) {
	programName := node.(Program).Name

	log.Printf("ENTER: PROGRAM %s\n", programName)
	ar := ActivationRecord{Name: programName, Type: ProgramType, NestingLevel: 1, Members: make(map[string]interface{})}
	itpr.CallStack.push(ar)
	log.Println(ar)
	_, err := itpr.visit(node.(Program).Block)
	if err != nil {
		return nil, err
	}
	log.Printf("LEAVE: PROGRAM %s\n", programName)
	log.Println(ar)
	itpr.CallStack.pop()

	return nil, nil
}

func (itpr *Interpreter) visitBlock(node AST) (interface{}, error) {
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

func (itpr *Interpreter) visitProcedureCall(node AST) (interface{}, error) {
	return nil, nil
}

func (itpr *Interpreter) visit(node AST) (interface{}, error) {
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
	case Program:
		return itpr.visitProgram(node)
	case Block:
		return itpr.visitBlock(node)
	case VarDecl:
		return itpr.visitVarDecl(node)
	case ProcedureDecl:
		return itpr.visitProcedureDecl(node)
	case Type:
		return itpr.visitType(node)
	case ProcedureCall:
		return itpr.visitProcedureCall(node)
	default:
		return nil, errors.New(
			fmt.Sprintf("(interpreter) Unknown node type %T", node))
	}
}

func (itpr Interpreter) interprete() (interface{}, error) {
	return itpr.visit(itpr.node)
}
