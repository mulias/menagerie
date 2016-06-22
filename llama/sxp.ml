(**
sxp.ml
A simple s-expression data type. While this s-expression implementation allows
for any kind of cons cells, llama does not allow pairs -- all cons must have
a cdr value that is another cons or null.
**)

open Core.Std

(**
Syntax
**)

type t =
  | Sym of string
  | Int of int
  | Float of float
  | Bool of bool
  | Str of string
  | Cons of t * t
  | Null

(**
To String
**)

(* String of the full s-expression, starting at the given sxp *)
let to_string (s : t) : string =
  let rec to_str (s : t) : string =
    match s with
    | Cons(Cons(x,y),Null) -> "("^(to_str x)^(to_str y)^")"
    | Cons(Cons(x,y),z)    -> "("^(to_str x)^(to_str y)^")"^(to_str z)
    | Cons(x,Null)         -> (to_str x)
    | Cons(x,y)            -> (to_str x)^(to_str y)
    | Sym x                -> " "^x^" "
    | Int x                -> " "^(Int.to_string x)^" "
    | Float x              -> " "^(Float.to_string x)^" "
    | Bool x               -> " "^(Bool.to_string x)^" "
    | Str x                -> " "^x^" "
    | Null                 -> " () "
  in 
  match s with
  | Cons _ -> "("^(to_str s)^")"
  | _      -> to_str s

(* String of the type of the current sxp *)
let to_type_string (s : t) : string =
  let rec to_str (s : t) : string =
    match s with
    | Cons(Cons _,Cons _) -> "Cons(Cons(...), Cons(...))"
    | Cons(Cons _, x)     -> "Cons(Cons(...), "^(to_str x)^")"
    | Cons(x, Cons _)     -> "Cons("^(to_str x)^", Cons(...))"
    | Cons(x, y)          -> "Cons("^(to_str x)^", "^(to_str y)^")"
    | Sym _               -> "Sym"
    | Int _               -> "Int"
    | Float _             -> "Float"
    | Bool _              -> "Bool"
    | Str _               -> "Str"
    | Null                -> "Null"
  in to_str s
