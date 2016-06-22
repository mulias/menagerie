package lexer

import (
	"unicode/utf8"

	"github.com/mulias/marmot/token"
)

//
// Lexer
// The Lexer divides a string into tokenized items, passing each token to the
// Parser by way of a channel. The details of the channel are hidden, so the
// Lexer is used by calling a lexing procedure as a goroutine, then retrieve
// each token with NextToken().
// Lexing does not raise errors -- all syntax and runtime errors are dealt with
// by the Parser and Evaluator respectively.
//
type Lexer struct {
	inputs      chan string     // get each input to lex
	tokens      chan token.Item // send each lexed token
	messageOut  chan string     // get massages from interpreter
	messageBack chan string     // send messages to interpreter
	str         string          // current input to lex
	length      int             // length of input string
	pos         int             // current position in input
	next        rune            // rune starting at pos
	width       int             // width of rune starting at pos
	runePeeked  bool            // have we looked at the rune starting at pos?
}

// we'll need to return eof as a rune
const eof = -1

func New(inputs chan string, tokens chan token.Item,
	messageOut chan string, messageBack chan string) Lexer {
	// no runes looked at, new channel
	return Lexer{
		inputs:      inputs,
		tokens:      tokens,
		messageOut:  messageOut,
		messageBack: messageBack,
	}
}

// lex string, sending tokens down Token channel
func (l Lexer) Lex() {
	for {
		switch m := <-l.messageOut; m {
		case "start!":
			l.messageBack <- "ready!"
			l.lexInput()
		case "end!":
			l.messageBack <- "done!"
		case "shutdown!":
			l.messageBack <- "exiting!"
			return
		}
	}
}

func (l *Lexer) lexInput() {
	// set up to lex an input
	in := <-l.inputs
	l.str = in
	l.length = len(in)
	l.pos = 0
	l.runePeeked = false
	// until input is done or error
	for {
		select {
		// abandon input
		case <-l.messageOut:
			l.messageBack <- "lexer: breaking"
			l.tokens <- token.New(token.EOF, "EOF")
			return
		// continue input
		default:
			t := l.lexToken()
			l.tokens <- t
			if t.Type == token.EOF {
				return
			}
		}
	}

}

// lex next token of input
func (l *Lexer) lexToken() token.Item {
	switch r := l.peekRune(); {
	// skip whitespace and comments, find next token
	case isSpace(r):
		l.nextRune()
		return l.lexToken()
	case r == ';':
		l.skipComment()
		return l.lexToken()
	// parenthesis
	case r == '(':
		l.nextRune()
		return token.New(token.LParen, "(")
	case r == ')':
		l.nextRune()
		return token.New(token.RParen, ")")
	// constant
	case r == '#':
		constant := l.lexConst()
		return token.New(token.Const, constant)
	// quote
	case r == '\'':
		l.nextRune()
		return token.New(token.Quote, "'")
	// end of file
	case r == eof:
		return token.New(token.EOF, "EOF")
	// otherwise symbol
	default:
		sym := l.lexSym()
		return token.New(token.Sym, sym)
	}
}

// return the rune starting at pos, don't advance pos
func (l *Lexer) peekRune() rune {
	switch {
	// if at end of input, next rune is eof
	case l.pos >= l.length:
		l.next = eof
		l.width = 0
	// if we haven't peeked at rune, get rune
	case !l.runePeeked:
		l.next, l.width = utf8.DecodeRuneInString(l.str[l.pos:])
		l.runePeeked = true
	}
	// next rune is set in switch, or if
	// we've looked before, value is the same
	return l.next
}

// advance pos by one rune
func (l *Lexer) nextRune() rune {
	// peek to get rune info
	l.peekRune()
	// move forward, next rune is unknown
	l.pos += l.width
	l.runePeeked = false
	return l.next
}

// proceed through input string until '\n' or eof is reached
// lexer position is advanced to in front of first unlexed rune
func (l *Lexer) skipComment() {
	for !isLineEnd(l.peekRune()) {
		l.nextRune()
	}
}

// For string constants, return substring from start position to end " of string
// For all other constants, return substring from start pos to last rune before
// the start of the next token. Lexer position is advanced to in front of first
// invalid sumbol rune
func (l *Lexer) lexConst() string {
	// we don't care about the leading #
	l.nextRune()
	start := l.pos
	if l.peekRune() == '"' {
		// it's a string
		l.scanStringConst()
	} else {
		// it's some other constant
		l.scanToNextToken()
	}
	return l.str[start:l.pos]
}

// Return substring from the start position to the last rune before a new token.
// Lexer position is advanced to in front of first invalid sumbol rune
func (l *Lexer) lexSym() string {
	start := l.pos
	l.scanToNextToken()
	return l.str[start:l.pos]
}

// Proceed through input until the next rune marks the start of a new token.
func (l *Lexer) scanToNextToken() {
	for !isTokenStartRune(l.peekRune()) {
		l.nextRune()
	}
}

// As long as the first rune is ", proceed through input until after the closing
// ", or until right before a new line or eof
func (l *Lexer) scanStringConst() {
	// check it's a string, should start with "
	// if it doesn't start with ", do nothing
	first := l.peekRune()
	if first == '"' {
		// go to next rune
		l.nextRune()
		// go until matching " is found or next rune ends the line or is eof
		for next := l.peekRune(); next != '"' && !isLineEnd(next); {
			l.nextRune()
			// if an escaped quote \" is found, skip it
			if next == '\\' && l.peekRune() == '"' {
				l.nextRune()
			}
			next = l.peekRune()
		}
		// if the string scan ended by finding a closing ", get that rune too
		if l.peekRune() == '"' {
			l.nextRune()
		}
	}
}

// true if rune is whitespace
func isSpace(r rune) bool {
	return r == ' ' || r == '\t' || r == '\r' || r == '\n'
}

// true if rune is end of line or end of file
func isLineEnd(r rune) bool {
	return r == eof || r == '\n'
}

// true if the next rune signifies the start of a new token
func isTokenStartRune(r rune) bool {
	return (isSpace(r) || r == ';' || r == '(' || r == ')' ||
		r == '#' || r == '\'' || r == eof)
}
