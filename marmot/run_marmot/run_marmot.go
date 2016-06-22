package marmot

import (
	"bufio"
	"fmt"
	"os"

	"github.com/mulias/marmot/ast"
	"github.com/mulias/marmot/lexer"
	"github.com/mulias/marmot/parser"
	"github.com/mulias/marmot/token"
)

// an instance of the marmot language
type Marmot struct {
	inputs1 chan string     // read input strings
	inputs2 chan string     // read input strings
	lex     lexer.Lexer     // lex input into tokens
	tokens1 chan token.Item // send tokens
	tokens2 chan token.Item // send tokens
	parse   parser.Parser   // parse tokens into trees
	trees1  chan ast.Cell   // send trees
	trees2  chan ast.Cell   // send trees
	//	eval        evaluator.Evaler    // evaluate tree into output
	//	env         map[string]ast.Cell // environment to eval in
	outputs      chan string // send output strings
	messageOut1  chan string // send message out to pipeline
	messageOut2  chan string // send message out to pipeline
	messageBack1 chan string // get messages back from pipeline
	messageBack2 chan string // get messages back from pipeline
}

// start a new marmot instance
func New() Marmot {
	m := Marmot{
		inputs1:      make(chan string),
		inputs2:      make(chan string),
		tokens1:      make(chan token.Item),
		tokens2:      make(chan token.Item),
		trees1:       make(chan ast.Cell),
		trees2:       make(chan ast.Cell),
		outputs:      make(chan string),
		messageOut1:  make(chan string),
		messageOut2:  make(chan string),
		messageBack1: make(chan string),
		messageBack2: make(chan string),
	}
	m.lex = lexer.New(m.inputs2, m.tokens1, m.messageOut2, m.messageBack1)
	m.parse = parser.New(m.tokens2, m.trees1, m.messageOut2, m.messageBack1)
	//		m.eval = evaluator.New(trees, outputs, messageOut, messageBack)
	return m
}

const eof = -1

func (m *Marmot) REPL() {

	go m.listenStr("Out: ", m.messageOut1, m.messageOut2)
	go m.listenStr("Back: ", m.messageBack1, m.messageBack2)
	go m.listenStr("In: ", m.inputs1, m.inputs2)
	go m.listenTok("Token: ", m.tokens1, m.tokens2)
	go m.listenAST("Tree: ", m.trees1, m.trees2)
	// start pipeline
	m.startup()

	// welcome
	fmt.Println("Welcome to Marmot")
	fmt.Println("Use Ctrl+D to Exit")
	fmt.Print("> ")

	// get input
	s := bufio.NewScanner(os.Stdin)
	for s.Scan() {
		// wait for clean starting pipeline, send input
		m.syncStart()
		m.inputs1 <- s.Text()
		// wait for output
	Loop:
		for {
			select {
			case message := <-m.messageBack2:
				if message == "error!" {
					break Loop
				}
				//			case out := <-m.outputs:
			case out := <-m.trees2:
				fmt.Println(out)
				if out.Type() == ast.EOF {
					break Loop
				}
			}
		}
		// wait for clean ending pipeline
		m.syncEnd()
		os.Stdout.Sync()
		fmt.Print("> ")
	}
	fmt.Println("exit")

	// wait for pipeline to close
	m.shutdown()
}

/*
func (m *Marmot) RunFile(filename string) {
	byts, err := ioutil.ReadFile(filename)
	if err != nil {
		panic(err)
	}
	m.startup()
	m.inputs <- string(byts)
	for out := <-m.outputs; out != eof; out = <-m.outputs {
		fmt.Println(out)
	}
	m.shutdown()
	os.Stdout.Sync()
}
*/

func (m *Marmot) startup() {
	go m.lex.Lex()
	go m.parse.Parse()
	//go m.eval.Eval()
}

func (m *Marmot) syncStart() {
	// block until all 3 pipeline members are ready to go
	for i := 0; i < 2; i++ {
		m.messageOut1 <- "start!"
		for ret := <-m.messageBack2; ret != "ready!"; ret = <-m.messageBack2 {
			m.messageOut1 <- "start!"
		}
	}
}

func (m *Marmot) syncEnd() {
	// block until all 3 pipeline members are done
	for i := 0; i < 2; i++ {
		m.messageOut1 <- "end!"
		for ret := <-m.messageBack2; ret != "done!"; ret = <-m.messageBack2 {
			m.messageOut1 <- "end!"
		}
	}
}

func (m *Marmot) shutdown() {
	// block until all 3 pipeline members are shutting down
	for i := 0; i < 2; i++ {
		m.messageOut1 <- "shutdown!"
		for ret := <-m.messageBack2; ret != "exiting!"; ret = <-m.messageBack2 {
			m.messageOut1 <- "shutdown!"
		}
	}
}

func (m *Marmot) listenStr(desc string, in chan string, out chan string) {
	for {
		str := <-in
		fmt.Println(desc, str)
		out <- str
	}
}

func (m *Marmot) listenTok(desc string, in chan token.Item, out chan token.Item) {
	for {
		str := <-in
		fmt.Println(desc, str)
		out <- str
	}
}

func (m *Marmot) listenAST(desc string, in chan ast.Cell, out chan ast.Cell) {
	for {
		str := <-in
		fmt.Println(desc, str)
		out <- str
	}
}
