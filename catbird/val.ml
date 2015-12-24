open Core.Std

(* VALUE *)
(* OUTPUT WE WANT TO PRODUCE *)
(******************************************************************************)

type t =
  | Int of int
  | Str of string
  | Bool of bool
  | Sym of string
  | Fun of (t -> tm)
  | List of t list
  | UV
and
tm = (t, string) Monads.Or_failure.t

let rec list_to_string (v : t) : string =
  match v with
  | Int i  -> Int.to_string i
  | Str s  -> "\"" ^ s ^ "\""
  | Bool b -> Bool.to_string b
  | Sym s  -> s
  | List l -> "(" ^ (String.concat (List.map l list_to_string) ~sep:" ") ^ ")" 
  | Fun _  -> "<function>"
  | UV     -> "unit"
  
let to_string (v : t) : string =
  match v with
  | Int i  -> Int.to_string i
  | Str s  -> "\"" ^ s ^ "\""
  | Bool b -> Bool.to_string b
  | Sym s  -> "'" ^ s
  | List l -> "'(" ^ (String.concat (List.map l list_to_string) ~sep:" ") ^ ")" 
  | Fun _  -> "<function>"
  | UV     -> "unit"

let tm_to_string (tm : tm) : string =
  match tm with
  | Monads.Or_failure.Success v -> to_string v
  | Monads.Or_failure.Failure e -> "Error: " ^ e