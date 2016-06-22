package ast

import "strconv"

//
// Integer
// Number info here.
//

// number expression contains an int
// implements Cell interface
type IntCell struct {
	value int64
	next  Cell
	sxp   *ListCell
}

// type of Num is always SxpNum
func (_ IntCell) Type() SxpType {
	return SxpInt
}

// integer value in cell
func (i IntCell) Value() int64 {
	return i.value
}

// string rep of integer
func (i IntCell) String() string {
	return strconv.FormatInt(i.value, 10)
}

// change next cell in list, overwrite old next value
// TODO: as above
func (i *IntCell) setNext(c Cell) Cell {
	i.next = c
	return c
}

// pointer to a IntCell with value i
func NewInt(s string) (*IntCell, error) {
	num, err := strconv.ParseInt(s, 10, 64)
	if err != nil {
		return nil, err
	}
	return &IntCell{value: num}, nil
}

// cell after num, or nil
func (i IntCell) Next() Cell {
	return i.next
}
