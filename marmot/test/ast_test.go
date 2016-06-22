package marmot_test

import (
	"fmt"
	"strconv"
	"testing"

	"github.com/mulias/marmot/ast"
)

func list1() (*ast.ListCell, *ast.ListCell) {
	l0 := ast.NewList()
	l0.Add(ast.NewSym("+"))
	l0.Add(ast.NewSym("x"))
	l1 := l0.Add(ast.NewList()).(*ast.ListCell)
	l1.Add(ast.NewSym("*"))
	l1.Add(ast.NewNum(4))
	l1.Add(ast.NewNum(7))
	l0.Add(ast.NewNum(1))
	return l0, l1
}

func list2() (*ast.ListCell,
	*ast.ListCell,
	*ast.ListCell,
	*ast.ListCell,
	*ast.ListCell) {
	// l0 is the top list, l1 and l2 are elements of l0
	// l3 and l4 are elements of l2
	l0 := ast.NewList()
	l0.Add(ast.NewNum(3))
	l1 := l0.Add(ast.NewList()).(*ast.ListCell)
	l2 := l0.Add(ast.NewList()).(*ast.ListCell)
	l3 := l2.Add(ast.NewList()).(*ast.ListCell)
	l2.Add(ast.NewSym("g"))
	l4 := l2.Add(ast.NewList()).(*ast.ListCell)
	return l0, l1, l2, l3, l4
}

func TestList(t *testing.T) {
	// expected results
	l0Val := "list #0"
	l0Cells := []string{"'+", "'x", "list #1", "1"}
	l1Cells := []string{"'*", "4", "7"}
	// make list
	l0, l1 := list1()
	// check outer list has correct value
	l0ValOut := fmt.Sprint(l0)
	if l0ValOut != l0Val {
		t.Errorf("Linked List %v, expected \"%s\", got \"%s\"",
			l0Cells, l0Val, l0ValOut)
	}
	// check outer list contains correct values
	errors := checkList(l0, l0Cells)
	for _, e := range errors {
		t.Errorf("Linked List %v, %s", l0Cells, e)
	}
	// check inner list contains correct values
	errors = checkList(l1, l1Cells)
	for _, e := range errors {
		t.Errorf("Linked List %v, %s", l1Cells, e)
	}
}

func checkList(list *ast.ListCell, expect []string) []string {
	errors := make([]string, 0)
	cell := list.First()
	// for each expected value
	for i, e := range expect {
		// error if value expected but list end
		if cell == nil {
			unreadStr := fmt.Sprint(expect[i:])
			errors = append(errors, "list terminated before cells "+unreadStr)
			break
		}
		// error if wrong cell contents
		cellStr := fmt.Sprint(cell)
		if cellStr != e {
			errors = append(errors,
				"expected cell value \""+e+"\", got \""+cellStr+"\"")
		}
		cell = cell.Next()
	}
	// error if all expected done but list not done
	if cell != nil {
		lenDiff := list.Value() - len(expect)
		errors = append(errors,
			"output too long by "+strconv.Itoa(lenDiff)+" cells")
	}
	return errors
}

func TestParent(t *testing.T) {
	// l0 is the top list, l1 and l2 are elements of l0
	// l3 and l4 are elements of l2
	l0, l1, l2, l3, l4 := list2()
	if l0.Parent() != nil {
		t.Errorf("Linked List, l0 parent should be nil, got l%d",
			l0.Parent().Value())
	}
	if l1.Parent().Value() != 0 {
		t.Errorf("Linked List, l1 parent should be l0, got l%d",
			l1.Parent().Value())
	}
	if l2.Parent().Value() != 0 {
		t.Errorf("Linked List, l2 parent should be l0, got l%d",
			l1.Parent().Value())
	}
	if l3.Parent().Value() != 2 {
		t.Errorf("Linked List, l3 parent should be l2, got l%d",
			l1.Parent().Value())
	}
	if l4.Parent().Value() != 2 {
		t.Errorf("Linked List, l4 parent should be l2, got l%d",
			l1.Parent().Value())
	}
}
