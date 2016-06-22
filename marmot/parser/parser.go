package parser

import (
	"errors"
	"fmt"
	"unicode/utf8"

	"github.com/mulias/marmot/ast"
	"github.com/mulias/marmot/token"
)

//
// Parser
// The Parser takes a series of Tokens, interprates the value of each token, and
// builds an Abstract Syntax Tree where each cell contains a token value.
//
// Programmer Feels: I considered building ASTs linearly instead recursively.
// The benefit would be that error checking could happen before parseing each
// token, instead of in between each AST. The downside is that recursivly
// calling functions to build subtrees inside of trees is easy, while keeping
// track of each nested subtree and the current subtree depth is hard and ugly.
// Most errors should happen during parsing, so by breaking immediatly when
// a parse error happens, and only checking for eval errors between building
// ASTs, implementation is cleaner, and error response should be minimally
// effected.
//

type Parser struct {
	tokens      chan token.Item // get each lexed token
	trees       chan ast.Cell   // send each finished ast
	messageOut  chan string     // get massages from interpreter
	messageBack chan string     // send messages to interpreter
	atEOF       bool
}

func New(tokens chan token.Item, trees chan ast.Cell,
	messageOut chan string, messageBack chan string) Parser {
	return Parser{
		tokens:      tokens,
		trees:       trees,
		messageOut:  messageOut,
		messageBack: messageBack,
	}
}

func (p Parser) Parse() {
	for {
		switch m := <-p.messageOut; m {
		case "start!":
			p.messageBack <- "ready!"
			p.parseTokenStream()
		case "end!":
			p.messageBack <- "done!"
		case "shutdown!":
			p.messageBack <- "exiting!"
			return
		}
	}
}

func (p Parser) parseTokenStream() {
	p.atEOF = false
	for {
		select {
		case <-p.messageOut:
			p.messageBack <- "parser: breaking"
			return
		default:
			tok := <-p.tokens
			if tok.Type == token.EOF {
				p.atEOF = true
			}
			tree, err := p.parseAST(tok)
			if err != nil {
				p.messageBack <- "error!"
				fmt.Println(err)
				if !p.atEOF {
					for tok = <-p.tokens; tok.Type != token.EOF; tok = <-p.tokens {
					}
				}
				return
			} else {
				p.trees <- tree
				if tree.Type() == ast.EOF {
					return
				}
			}
		}
	}
}

func (p *Parser) parseAST(t token.Item) (ast.Cell, error) {
	var val ast.Cell
	var err error
	switch t.Type {
	// use following tokens to recursivly build the tree
	case token.LParen:
		val, err = p.parseTree(ast.NewList())
	// this "tree" is a single sym
	case token.Sym:
		val, err = ast.NewSym(t.Value)
	// this "tree" is a single constant
	case token.Const:
		val, err = p.parseConst(t)
	// the next token is quoted
	case token.Quote:
		val, err = p.parseQuote(t)
	// close paren found without open paren, error
	case token.RParen:
		val, err = nil, errors.New("syntax error: close parenthese ')' without s-expression")
	case token.EOF:
		val, err = ast.NewEOF(), nil
	}
	return val, err
}

func (p *Parser) parseTree(tree *ast.ListCell) (ast.Cell, error) {
	for {
		switch t := <-p.tokens; t.Type {
		// tree is done
		case token.RParen:
			return tree, nil
		// EOF found before tree is done, error
		case token.EOF:
			return ast.NewEOF(), fmt.Errorf(
				"syntax error: hanging parenthese, s-expression %v does not close", tree)
		default:
			val, err := p.parseAST(t)
			if err != nil {
				return val, err
			}
			tree.Add(val)
		}
	}
}

func (p *Parser) parseConst(t token.Item) (ast.Cell, error) {
	var val ast.Cell
	var err error
	switch r, _ := utf8.DecodeRuneInString(t.Value); {
	// string
	case r == '"':
		val, err = ast.NewStr(t.Value)
	// char
	case r == '\\':
		val, err = ast.NewChar(t.Value[1:])
	// intager
	case r == 'i':
		val, err = ast.NewInt(t.Value[1:])
	// decimal
	case r == 'd':
		val, err = ast.NewFlo(t.Value[1:])
	// boolean
	case r == 't' || r == 'f' || r == '?':
		val, err = ast.NewBool(t.Value)
	// else error
	default:
		err = fmt.Errorf("syntax error: %s is not a valid constant", t.Value)
	}
	return val, err
}

func (p *Parser) parseQuote(t token.Item) (ast.Cell, error) {
	qList := ast.NewList()
	qCell, err1 := ast.NewSym("quote")
	val, err2 := p.parseAST(<-p.tokens)
	if err1 != nil {
		return nil, err1
	}
	if err2 != nil {
		return nil, err2
	}
	qList.Add(qCell)
	qList.Add(val)
	return qList, nil
}
