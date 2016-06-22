package ast

import (
	"fmt"
	"math/rand"
)

//
// Boolean
// Info about symbols here
//

// implements Cell interface
type BoolCell struct {
	value    bool
	confused bool
	next     Cell
}

// type of Sym is always SxpBool
func (_ BoolCell) Type() SxpType {
	return SxpBool
}

// string representation of value
func (b BoolCell) Value() bool {
	if b.confused {
		return rand.Float32() < .5
	} else {
		return b.value
	}
}

// return with leading #
func (b BoolCell) String() string {
	var s string
	switch {
	case b.confused:
		s = "#???"
	case b.value:
		s = "#true"
	default:
		s = "#false"
	}
	return s
}

// cell after boolean, or nil
func (b BoolCell) Next() Cell {
	return b.next
}

// change next cell in list, overwrite old next value
// TODO: as above
func (b *BoolCell) setNext(c Cell) Cell {
	b.next = c
	return c
}

// pointer to a BoolCell, set symbol value
func NewBool(s string) (*BoolCell, error) {
	var b *BoolCell
	var err error
	switch {
	case s == "t" || s == "true":
		b = &BoolCell{value: true}
	case s == "f" || s == "false":
		b = &BoolCell{value: false}
	case s == "?" || s == "???":
		b = &BoolCell{confused: true}
	default:
		err = fmt.Errorf("syntax error: %s is not a valid constant", s)
	}
	return b, err
}
