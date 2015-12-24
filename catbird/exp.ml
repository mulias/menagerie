open Core.Std

(* EXP *)
(* THE AST WE BUILD THEN INTERPRET *)
(******************************************************************************)

type const =
  | Int of int
  | Str of string
  | Bool of bool
  | Sym of string
  | List of const list
  | UE

type t =
  | Const of const
  | Var of string
  | Lambda of t * t 
  | Apply of t * t
  | Define of t * t
  | Let of t * t * t

let rec const_to_string (const : const) : string =
  match const with
  | Int i  -> "Int "  ^ Int.to_string i 
  | Str s  -> "Str "  ^ "\"" ^ s ^ "\""
  | Bool b -> "Bool " ^ Bool.to_string b
  | Sym s  -> "Sym "  ^ "\"" ^ s ^ "\""
  | UE     -> "unit"
  | List l -> "List [" ^ (String.concat (List.map l const_to_string) ~sep:";") ^ "]"  

let rec to_string (exp : t) : string =
  match exp with
  | Const c -> "Const (" ^ (const_to_string c) ^ ")"
  | Var x   -> "Var " ^ "\"" ^ x ^ "\""
  | Lambda (v, body) -> "Lambda (" ^ (to_string v) ^ ", " ^ (to_string body) ^ ")"
  | Apply (f, arg) -> "Apply (" ^ (to_string f) ^ ", " ^ (to_string arg) ^ ")"
  | Define (v, e) -> "Define (" ^ (to_string v) ^ ", " ^ (to_string e) ^ ")"
  | Let (v, e, body) -> "Let (" ^ (to_string v) ^ ", " ^ (to_string e) ^ ", "
      ^ (to_string body) ^ ")"
