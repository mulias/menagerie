package ast

import (
	"fmt"
	"strings"
)

//
// Symbol
// Info about symbols here
//

// symbol cell contains a string
// implements Cell interface
type SymCell struct {
	value string
	next  Cell
}

// type of Sym is always SxpSym
func (_ SymCell) Type() SxpType {
	return SxpSym
}

// string representation of value
func (s SymCell) Value() string {
	return s.value
}

func (s SymCell) String() string {
	return s.value
}

// cell after symbol, or nil
func (s SymCell) Next() Cell {
	return s.next
}

// change next cell in list, overwrite old next value
// TODO: as above
func (s *SymCell) setNext(c Cell) Cell {
	s.next = c
	return c
}

// pointer to a SymCell, set symbol value
func NewSym(s string) (*SymCell, error) {
	// check for invalid chars
	invalid := []rune{'"', '\\'}
	for _, r := range invalid {
		if strings.ContainsRune(s, r) {
			err := fmt.Errorf("syntax error: character %v invalid in symbol %s", r, s)
			return nil, err
		}
	}
	// everything is ok
	return &SymCell{value: s}, nil
}
