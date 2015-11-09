type exp =
  | Int of int
  | Str of string
  | Bool of bool
  | Var of string
  | Lambda of exp * exp
  | Apply of exp * exp
  | If of exp * exp * exp

type out =
  | Int of int
  | Str of string
  | Bool of bool
  | Fun of (out -> out)
  | Error

val run : exp -> out
