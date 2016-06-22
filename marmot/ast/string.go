package ast

import (
	"fmt"
	"unicode/utf8"
)

//
// Symbol
// Info about symbols here
//

// implements Cell interface
type StringCell struct {
	value string
	next  Cell
}

// type of Sym is always SxpSym
func (_ StringCell) Type() SxpType {
	return SxpStr
}

// string representation of value
func (s StringCell) Value() string {
	return s.value
}

func (s StringCell) String() string {
	return s.value
}

// cell after string, or nil
func (s StringCell) Next() Cell {
	return s.next
}

// change next cell in list, overwrite old next value
// TODO: as above
func (s *StringCell) setNext(c Cell) Cell {
	s.next = c
	return c
}

// pointer to a StringCell, set symbol value
func NewStr(s string) (*StringCell, error) {
	if r, _ := utf8.DecodeLastRuneInString(s); r != '"' {
		err := fmt.Errorf("syntax error: string \" %s \" does not end in \"", s)
		return nil, err
	}
	return &StringCell{value: s}, nil
}
