package ast

//
// Abstract Syntax Tree
// The marmot AST is implemented as a linked list using cons cells. Each cons
// cell contains an s-expression -- either an atomic symbol, or a new linked
// list of s-expressions. The final cell in the list points to nil.
// Because list is a kind of cell, the entire AST is contained in one cell,
// which points to nil. This cell contains the list of all s-expressions. The
// Add(cell) method adds a new cell to the end of a list. It is not called on
// the last list element, but on the list cell itself.
//

// cons cells implement Cell, but each kind contains a different data type
type Cell interface {
	Type() SxpType       // type of data in cell
	Next() Cell          // next cell in linked list
	setNext(n Cell) Cell // change next cell in linked list
	String() string      // to implement fmt
	// each implementation has a Value(), with varying return types
}

// possible types for s-expression in cell
type SxpType int

const (
	SxpList SxpType = iota // linked list
	SxpSym                 // lisp symbol
	SxpInt                 // integer constant
	SxpFlo                 // floating point constant
	SxpBool                // boolean constant
	SxpChar                // character constant
	SxpStr                 // string constant
	EOF                    // signal all asts processed
)

// I don't want to grace EOF with it's own file, so I'll define it here
type EOFCell struct {
}

func (_ EOFCell) Type() SxpType {
	return EOF
}

func (_ EOFCell) Value() int {
	return -1
}

func (_ EOFCell) Next() Cell {
	return nil
}

// this is probably a bad idea
func (_ EOFCell) setNext(n Cell) Cell {
	return n
}

func (_ EOFCell) String() string {
	return "EOF"
}

func NewEOF() *EOFCell {
	return &EOFCell{}
}
