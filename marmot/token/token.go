package token

//
// Tokens
// The 1985 LISP 1.5 Programmer's Manual gives this definition of Lisp grammar,
// which I find pleasing:
// "An S-expression is either an atomic symbol or it is composed of these
// elements in the following order: a left parenthesis, an S-expression, a dot,
// an S-expression, and a right parenthesis."
// Usually Lisp implementations make the dot optional, I'm ignoring it for
// simplicity. Like any actually useful Lisp, I'm adding constant data types
// such as numbers. So a token is either a left paren, a right paren, or an
// atom, which is either a symbol or a constant. On top of those, the EOF token
// is used to signal the end of input.
// Individual tokens are called Items to avoid clashing with package go/Token
//
type Item struct {
	Type  ItemType // type of token
	Value string   // value of token
}

type ItemType int

const (
	LParen ItemType = iota
	RParen
	Sym
	Const
	Quote
	EOF
)

// concise way to make tokens
func New(t ItemType, v string) Item {
	return Item{Type: t, Value: v}
}

// implement fmt
func (i Item) String() string {
	return i.Value
}
