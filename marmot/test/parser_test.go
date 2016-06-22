package marmot_test

import (
	"strconv"
	"testing"

	"github.com/mulias/marmot/parser"
)

func TestParse(t *testing.T) {
	in := []string{"(", "+", "3", "(", "*", "4", "7", ")", "1", ")"}
	inStr := "(+ 3 (* 4 7) 1)"
	// make parser, parse
	p := parser.NewParser()
	out := p.ParseFromSlice(in)
	// make expected
	s0 := parser.NewSxp()
	s0.Add(parser.NewSym("+"))
	s0.Add(parser.NewNum(3))
	cs1 := s0.Add(parser.NewSxp())
	s1 := cs1.(*parser.SxpCell)
	s1.Add(parser.NewSym("*"))
	s1.Add(parser.NewNum(4))
	s1.Add(parser.NewNum(7))
	s0.Add(parser.NewNum(1))
	expect := s0
	// compare ASTs
	errors := compareParsed(expect, out, "")
	// list errors
	for _, e := range errors {
		t.Errorf("%s, input: %s", e, inStr)
	}
}

func compareParsed(expect *parser.SxpCell, out *parser.SxpCell, posStr string) []string {
	e := expect.First()
	o := out.First()
	pos := 0
	errors := make([]string, 0)
	// until at least one list ends
	for e != nil && o != nil {
		switch {
		// if both are s-epressions, recursive call
		case e.Type() == parser.ExpSXP && o.Type() == parser.ExpSXP:
			newErrors := compareParsed(e.(*parser.SxpCell), o.(*parser.SxpCell),
				posStr+":"+strconv.Itoa(pos))
			errors = append(errors, newErrors...)
		// error if only one is an s-expression
		case e.Type() == parser.ExpSXP && o.Type() != parser.ExpSXP:
			errors = append(errors,
				"Parser expected s-expression, got "+o.String()+", pos "+posStr)
		// error if only one is an s-expression
		case e.Type() != parser.ExpSXP && o.Type() == parser.ExpSXP:
			errors = append(errors,
				"Parser expected "+e.String()+", got s-expression, pos "+posStr)
			// error if type or data does not match
		case compareCells(e, o):
			errors = append(errors,
				"Parser expect "+e.String()+" got "+o.String()+", pos "+posStr)
		}
		e = e.Next()
		o = o.Next()
		pos += 1
	}
	if e != nil {
		errors = append(errors, "Parser output shorter than expected")
	}
	if o != nil {
		errors = append(errors, "Parser output longer than expected")
	}
	return errors
}

// true if cells contain expressions of the same type and value
func compareCells(a parser.Cell, b parser.Cell) bool {
	// if same type, check value
	if a.Type() == b.Type() {
		switch t := a.Type(); t {
		case parser.ExpSXP:
			return a.(*parser.SxpCell).Value() == b.(*parser.SxpCell).Value()
		case parser.ExpSYM:
			return a.(*parser.SymCell).Value() == b.(*parser.SymCell).Value()
		case parser.ExpNUM:
			return a.(*parser.NumCell).Value() == b.(*parser.NumCell).Value()
		}
	}
	// if not the same type
	return false
}
