package marmot_test

import (
	"testing"

	"github.com/mulias/marmot/lexer"
	"github.com/mulias/marmot/token"
)

func getTokens(input string) []string {
	l := lexer.New()
	go l.LexString(input)
	output := make([]string, 0)
	for {
		t := l.NextToken()
		output = append(output, t.Value)
		if t.Type == token.EOF {
			break
		}
	}
	return output
}

type TokenizeCase struct {
	in     string
	expect []string
}

func TestLexString(t *testing.T) {
	cases := []TokenizeCase{
		// nothing, spaces
		{"", []string{"EOF"}},
		{"    ", []string{"EOF"}},
		{"\t \r ", []string{"EOF"}},
		// comments
		{";", []string{"EOF"}},
		{";()())))", []string{"EOF"}},
		{"; (atom . 324(___))  ", []string{"EOF"}},
		{";2\n()", []string{"(", ")", "EOF"}},
		{"; (((**))) \n (atom . 234)  ", []string{"(", "atom", ".", "234", ")", "EOF"}},
		// parens
		{"(", []string{"(", "EOF"}},
		{")", []string{")", "EOF"}},
		{"(()())", []string{"(", "(", ")", "(", ")", ")", "EOF"}},
		{"( (   ", []string{"(", "(", "EOF"}},
		{" ) ) ", []string{")", ")", "EOF"}},
		{")))((", []string{")", ")", ")", "(", "(", "EOF"}},
		{"   (   ", []string{"(", "EOF"}},
		// atoms, special characters
		{"atom", []string{"atom", "EOF"}},
		{"atom   atom ", []string{"atom", "atom", "EOF"}},
		{"123", []string{"123", "EOF"}},
		{"123 654   12", []string{"123", "654", "12", "EOF"}},
		{"a12 i34l  _atom ", []string{"a12", "i34l", "_atom", "EOF"}},
		// everything
		{"(_?+(-*\t \\).\r ) ;words", []string{"(", "_?+", "(", "-*", "\\", ")", ".", ")", "EOF"}},
	}
	for _, c := range cases {
		out := getTokens(c.in)
		// if slices are not same length, iterate over shorter one
		var shortSlice []string
		if len(c.expect) > len(out) {
			shortSlice = out
		} else {
			shortSlice = c.expect
		}
		for i, _ := range shortSlice {
			if c.expect[i] != out[i] {
				t.Errorf("Lexer input \"%s\", expect %v, got %v, error at token %d",
					c.in, c.expect, out, i)
			}
		}
		if len(c.expect) > len(out) {
			t.Errorf("Lexer input \"%s\", expect %s, got %s, missing tokens",
				c.in, c.expect, out)
		}
		if len(c.expect) < len(out) {
			t.Errorf("Lexer input \"%s\", expect %s, got %s, extra tokens",
				c.in, c.expect, out)
		}
	}
}
