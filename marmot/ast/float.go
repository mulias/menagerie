package ast

import "strconv"

//
// Number
// Number info here.
//

// number expression contains an int
// implements Cell interface
type FloCell struct {
	value float64
	next  Cell
	sxp   *ListCell
}

func (_ FloCell) Type() SxpType {
	return SxpFlo
}

func (f FloCell) Value() float64 {
	return f.value
}

func (f FloCell) String() string {
	return strconv.FormatFloat(f.value, 'f', 15, 64)
}

// change next cell in list, overwrite old next value
// TODO: as above
func (f *FloCell) setNext(c Cell) Cell {
	f.next = c
	return c
}

// cell after num, or nil
func (n FloCell) Next() Cell {
	return n.next
}

// pointer to a FloCell with value i
func NewFlo(s string) (*FloCell, error) {
	num, err := strconv.ParseFloat(s, 64)
	if err != nil {
		return nil, err
	}
	return &FloCell{value: num}, nil
}
