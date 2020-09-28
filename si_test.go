package main

import (
	"log"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestSymbol(t *testing.T) {
	table := NewSymbolTable()

	varXSymbol := Symbol{"x", intType}
	varYSymbol := Symbol{"y", realType}
	table.define(varXSymbol)
	table.define(varYSymbol)
	log.Println(table)

	resX, ok := table.lookup("x")
	assert.True(t, ok)
	assert.Equal(t, resX, varXSymbol)
	assert.NotEqual(t, resX, varYSymbol)

	resY, ok := table.lookup("y")
	assert.Equal(t, resY, varYSymbol)
}
