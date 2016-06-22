package main

import "github.com/mulias/marmot/run_marmot"

//
// Marmot
// Parse command line arguments, start a new instance of marmot and execute
// marmot code.
//
func main() {
	m := marmot.New()
	m.REPL()
}
