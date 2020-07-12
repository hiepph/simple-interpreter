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
	}
	for _, test := range tests {
		v, err := expr(test.text)
		if assert.NoError(t, err, test.text) {
			assert.Equal(t, v, test.want)
		}
	}
}
