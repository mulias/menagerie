The lexer [`lexer.mll`] creates tokens which are translated by the parser
[`parser.mly`] into an abstract syntax tree of expressions [`exp.ml`]. 
The interpreter [`interpreter.ml`] traverses the AST and produces an output
value [`value.ml`]. As it works, the interpreter saves and accesses variables in
an environment [`env.ml`], and keeps track of the environment and reports 
semantic errors [`monads.ml`]. Basic functions such as numeric and boolean 
algebra and list operations are defined in `expanded_env.ml`. Everything is
pulled together in `catbird.ml`, and can be
repl.ml
test.ml
catbird.ml
