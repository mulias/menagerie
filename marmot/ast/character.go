package ast

import (
	"fmt"
	"unicode/utf8"
)

//
// Symbol
// Info about symbols here
//

// symbol cell contains a string
// implements Cell interface
type CharCell struct {
	value rune
	next  Cell
}

// type of Sym is always SxpSym
func (_ CharCell) Type() SxpType {
	return SxpChar
}

// string representation of value
func (s CharCell) Value() rune {
	return s.value
}

func (c CharCell) String() string {
	return string(c.Value())
}

// cell after char, or nil
func (c CharCell) Next() Cell {
	return c.next
}

// change next cell in list, overwrite old next value
// TODO: as above
func (ch *CharCell) setNext(c Cell) Cell {
	ch.next = c
	return c
}

// pointer to a CharCell, set symbol value
func NewChar(s string) (*CharCell, error) {
	// check only one rune
	if l := utf8.RuneCountInString(s); l != 1 {
		err := fmt.Errorf("syntax error: character %s has length %d, length must be 1",
			s, l)
		return nil, err
	}
	c, _ := utf8.DecodeRuneInString(s)
	return &CharCell{value: c}, nil
}
