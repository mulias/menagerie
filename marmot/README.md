Marmot
======

The Go mascot is a gopher, a category which covers a large variety of different rodent species. Gophers _are not_ marmots. Groundhogs _are_ marmots. Groundhogs are apparently a completely different family from gophers. I have no idea what the practical difference is between a gopher and a groundhog.

Marmot is my implementation of a LISP, written in Go. I have never used Common Lisp before, but I have decent school experience in Scheme. I have never used Go before, but I have decent school experience in C.

There are a number of github projects named Marmot, but I can almost guarantee that none of them are as incomplete or poorly implemented as mine.

The Plan
--------
Marmot is in development. At this point I have a strong idea of what I want to do, but some details are fuzzy. 

From a high level perspective, Marmot uses a standard Go [pipeline pattern](http://blog.golang.org/pipelines) to turn text into runnable lisp, then back into text output. The pipeline looks like this:
```
                                      ||
                                      \/
                            +--------------------+
   +------Input String------| Marmot Interpreter | <------Output String------+
   |                        +--------------------+                           |
   |                                                                         |     
   |             +-----+          +-------+          +------+                |
   +-----------> | Lex |-Tokens-> | Parse |--ASTs--> | Eval |----------------+
                 +-----+          +-------+          +------+

```

The entire pipeline is contained in the marmot interpreter struct, located in run_marmot.go
The inputted strings, lexed tokens, parsed abstract syntax trees, and evaled output strings are all passed down one-directional channels, moving from one stage of the pipeline to the next. Each channel passes both relevant data for that point in the pipeline, and also metadata to communicate errors and the start and end of input.

Documentation
-------------

###Syntax
Marmot is designed to be implementation friendly first, user friendly second. Much of Marmot syntax is stripped down Racket syntax. In particular, Marmot requires that all constants start with a `#` character, and maybe another identifier, before the actual data. Constant data types include boolean, string, character, integer, and decimal. Symbols and lists are not constant types, and have different meaning when quoted or unquoted, as in any typical lisp. 

###Data Types
Boolean constants are either `#t` for true, or `#f` for false. There is a third boolean value, `#?` for confusion, which has a 50/50 chance of being true or false. You probably shouldn't use it.

String constants start and end with double quotes, and can contain any characters.
`#"this is an example string"`
`#"this is \"also\" valid"`
`#"this !@#$%^ \n \t ''' is ☎☎☎☎ fine"`

Character constants start with `#\` followed by a single unicode character. `#\q`, `#\\`, `#\@`, `#\☎`  are all valid characters

Integer constants start with `#i`, can optionally start with a `-` for negative, contain numeric characters 0-9, and can use e or E for scientific notation. `#i123`, `#i23E5`, `#i0`, `#i-6` are example integers.

Floating Point constants start with `#d` for decimal and use the same rules as ints, with the addition of an optional decimal point. `#d.345`, `#d7.054`, `#d2` are example decimals.

Symbols can not contain the characters  `'`, `"`, `#`, `\`, `;`, `(`, or `)`. Otherwise anything goes. An unquoted symbol is a reference to a definition saved in the environment, while a quoted symbol is a meaningless variable equal to itself. `sym` is unquoted, while `'sym` is quoted. 

Lists start with `(` and end with `)`. An unquoted list is expected to contain a symbol that references a function, followed by the arguments for that function. A quoted list can contain any data, which is pulled from the list using `car` and `cdr`, as in any typical lisp. Symbols and lists inside of a quoted list are automatically quoted, so it is not necessary to quote inside elements. `(eq? 'a 'b)` is an unquoted list. `'(a b c d)` is a quoted list. `'('a 'b '('cat 'dog '() '()))` is fine, but `'(a b (cat doge () ()))` works just as well. The empty list '() is called the null list. Null is not a data type, but the function `null` returns '().

###Functions
Marmot uses `func` instead of the traditional `lambda` to declare functions. I've heard it said that strictly speaking Lisp uses procedures, not functions, but I happen to like the sound of `func` more than `proc`.
I plan on implementing an optional type declaration syntax for functions, which will look like this:
```
; prependIota prepends the ints from 0 to i to list l
; the type declaration shows that it takes a list and an int, and returns a list
(define prependIota
  (func-> (%list %int %list) (l i)
    (cond ((eq? i 0) (cons 0 l))
          (#t (iota (- i 1) (cons i l))))))
```
This is not so much a declaration system as it is a check system, because it does nothing more than check at runtime that the input and output of the function match what is expected. [Typed Racket](http://docs.racket-lang.org/ts-guide/) uses an actual type system, and is pretty cool. I don't think this check system is necessary in most cases, but it has some uses for implementing basic language functions. The types recognized by func-> are: %sym, %list, %bool, %str, %char, %int, %dec, %rat, %num, %func, %any

###Example Marmot
Here's some example Marmot code. It's not the best way to solve the given problem, but it exhibits the properties I've outlined and has lots of comments for clarity. 
```
; indexTrue finds the index of the first element of l that is #t for test
; if all elements of l are #f for test, then return '()
; test is a function that applies a binary test to a value, returning #t or #f
; l is a list of any kind of elements
(define indexTrue
  (func (test l) 
    (indexFirst #t (map f l))))

; indexFirst finds the index of the first value in l that is equal to match 
; if no elements of l equal match, return '()
; match is a value to search for
; l is a list of any elements to search through
(define indexFirst
  (func (match l) 
    (indexFirstKernel match l #i0)))

; implement recursive part of indexFirst
; match is a value to search for
; l is a list of any elements to search through
; i is the current position in the list, 0 indexed
(define indexFirstKernel
  (func (match l i) 
    (cond ((eq? l '()) '())
          ((eq? match (car l)) i)
          (#t (indexFirstKernel match (cdr l) (+ i #i1))))))

; map applies f to each element of l
; f is a function that applies to all elements of l
; l is a list of elements f can be applied to
; function type declarations are optional
(define map
  (func-> (%func %list %list) (f l) 
    (mapKernel f '() l))))

; implement recursive part of map
; f is a function that applies to all elements of old
; old is a list to apply f to
; new is a list f has been applied to
; function type declarations are optional
(define mapKernel
  (func-> (%func %list %list %list) (f new old)
    (cond ((eq? old '()) new)
          (#t (mapKernel f (cond (f (car old)) new) (cdr old))))))

; use indexTrue
(indexTrue (func (x) (eq? x #i1)) '(#"string" 'sym #t #d1.1 #i1 #r3/5))
; this call returns 4, because integer 1 is at index 4
```

Sources
-------
* Some basic ideas from [Daniel Holden's ebook on writing a LISP in C](http://buildyourownlisp.com)
* Using channels and basic lexer structure from [Rob Pike's excellent talk on Lexical Scanning in Go](https://www.youtube.com/watch?v=HxaD_trXwRE) and [lexer source code](https://golang.org/src/text/template/parse/lex.go) referenced in the talk
* A formal definition of LISP by [MIT, 1985](http://www.softwarepreservation.org/projects/LISP/book/LISP%201.5%20Programmers%20Manual.pdf)
* A less formal definition of LISP by [Paul Graham, 2002](http://paulgraham.com/rootsoflisp.html)
* The chapter on Metalinguistic Abstraction in [The Wizard Book](https://mitpress.mit.edu/sicp/)
