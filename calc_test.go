package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestExpr(t *testing.T) {
	tests := []struct {
		text string
		want int
	}{
		{"3+5", 8},
		{"13+5", 18},
		{"13 + 5", 18},
		{"  13 +    5", 18},
		{"12 - 24", -12},
		{"12 - 24 + 6-37", -43},
		{"7 * 2 + 8 - 2 / 2 + 3 * 10 / 2", 36},
		{"7 * 2 + 8 - 2 / 2 + 3 * 10 / 2", 36},
		{"2 * (7 + 3)", 20},
		{"7 + 3 * (10 / (12 / (3 + 1) - 1))", 22},
		{"7 + (((3+2)))", 12},
	}
	for _, test := range tests {
		node, err := interprete(test.text)
		assert.NoError(t, err, test.text)

		v, err := visit(node)
		if assert.NoError(t, err, test.text) {
			assert.Equal(t, test.want, v, test.text)
		}
	}
}
