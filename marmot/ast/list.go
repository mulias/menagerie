package ast

//
// List
// Because list is a kind of cell, the entire AST is contained in one cell,
// which points to nil. This cell contains the list of all s-expressions. The
// Add(cell) method adds a new cell to the end of a list. It is not called on
// the last list element, but on the list cell itself.
// The ast is a singly linked list, with one exception -- each list cell points
// back to its parent list. The top list points to nil. This structure helps in
// recursivly evaluating lists.
// Here's an example ast, where arrows to the right are next cells, arrows down
// are contained lists, and arrows up and left are parent lists:
//
//    nil <------+
//               |
//           +-------+
//           | List1 |--> nil
//           +-------+<------------------+
//               |                       |
//               V                       |
//           +-------+   +-------+   +-------+
//           | 'sym  |-->|  435  |-->| List2 |--> nil
//           +-------+   +-------+   +-------+<----- +
//                                       |           |
//                                       V           |
//                                   +-------+   +-------+   +-------+
//                                   | 'sym  |-->| List3 |-->|   7   |--> nil
//                                   +-------+   +-------+   +-------+
//

// linked list cell contains pointers to list elements
// implements Cell interface
type ListCell struct {
	parent *ListCell // list this list belongs to
	index  int       //unique list id, top list is 0, contained lists index++
	subln  int       // number of sub lists contained in this list
	next   Cell      // cell after sxp
	first  Cell      // first cell inside of sxp
	last   Cell      // end of sxp linked list
	length int       // number of cells in sxp
}

// pointer to ListCell
func NewList() *ListCell {
	return new(ListCell)
}

// type of list is always SxpList
func (_ ListCell) Type() SxpType {
	return SxpList
}

// unique index of list in ast
func (l ListCell) Value() int {
	return l.index
}

// print unique index of list in ast
func (l ListCell) String() string {
	str := "( "
	for c := l.first; c != nil; c = c.Next() {
		str = str + c.String() + " "
	}
	return str + ")"
}

// length of list in cons cells, sub-lists are one cell
func (l ListCell) Length() int {
	return l.length
}

// cell after list, or nil if end
func (l ListCell) Next() Cell {
	return l.next
}

// change next cell after list
// TODO: change this to append, check that current next is nil, add nil after
func (l *ListCell) setNext(c Cell) Cell {
	l.next = c
	return c
}

// first cell in list, or nil if empty
func (l ListCell) First() Cell {
	return l.first
}

// last cell in list, or nil if empty
func (l ListCell) Last() Cell {
	return l.last
}

// the list this list is contained in, or nil if top list
func (l ListCell) Parent() *ListCell {
	return l.parent
}

// add a new cell to end of list, new cell points to nil
func (l *ListCell) Add(c Cell) Cell {
	if c.Type() == SxpList {
		// unpack list, set list's parent and index
		// l has one more sub-list
		l.subln += 1
		list := c.(*ListCell)
		list.parent = l
		list.index = list.parent.index + list.parent.subln
		c = list
	}
	if l.first == nil {
		l.first = c
		l.last = c
	} else {
		l.last.setNext(c)
		l.last = c
	}
	c.setNext(nil)
	l.length += 1
	return c
}
