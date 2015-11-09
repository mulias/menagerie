open Core.Std

(* AST AND OUTPUT TYPES *)

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

(* HELLO ASSOC, MY OLD FRIEND *)

type env = (exp, out) List.Assoc.t

(* HOW TO INTERPRET AN EXP IN AN ENV *)

let rec interp (exp : exp) (env : env) : out =
  let lookup (exp : exp) : out =
    match List.Assoc.find env exp with
    | Some out -> out
    | _ -> Error
  in let extend (arg : exp) (body :exp) : out =
    match arg with
    | Var _ -> Fun (fun x -> (interp body ((arg, x) :: env)))
    | _ -> Error
  in let apply (lambda : exp) (exp : exp) : out =
    match interp lambda env with
    | Fun f -> f (interp exp env)
    | _ -> Error
  in let if_consequent (test : exp) (res : exp) (alt : exp) : out =
    match interp test env with
    | Bool true  -> interp res env
    | Bool false -> interp alt env
    | _ -> Error
  in match exp with
  | Int i               -> Int i
  | Str s               -> Str s
  | Bool b              -> Bool b
  | Var _               -> lookup exp
  | Lambda (arg, body)  -> extend arg body
  | Apply (lambda, exp) -> apply lambda exp
  | If (test, res, alt) -> if_consequent test res alt

(* AWESOME, NOW WE CAN STEAL BASIC FUNCTIONS FROM OCAML *)

(* FUNCTION BUILDING *)

let unary_fun (from_v : (out -> 'a option)) (f : ('a -> 'b))
              (to_v : ('b -> out)) : out =
  Fun (fun (o1 : out) -> 
    match from_v o1 with
    | Some a -> to_v (f a)
    | None -> Error)

let binary_fun (from_v1 : (out -> 'a option)) (from_v2 : (out -> 'b option))
               (f : ('a -> 'b -> 'c)) (to_v : ('c -> out)) : out =
  Fun (fun (o1 : out) -> Fun (fun (o2 : out) -> 
    match (from_v1 o1, from_v2 o2) with
    | (Some a, Some b) -> to_v (f a b)
    | (None, _) -> Error
    | (_, None) -> Error))

let from_int (out : out) : int option =
  match out with
  | Int a -> Some a
  | _ -> None

let from_str (out : out) : string option =
  match out with
  | Str s -> Some s
  | _ -> None

let from_bool (out : out) : bool option =
  match out with
  | Bool b -> Some b
  | _ -> None

let to_int (i : int) : out = Int i

let to_str (s : string) : out = Str s

let to_bool (b : bool) : out = Bool b

(* MATH *)
let int_unary_op (op : (int -> int)) : out =
  unary_fun from_int op to_int
let int_binary_op (op : (int -> int -> int)) : out =
  binary_fun from_int from_int op to_int
let int_add  = int_binary_op ( + ) 
let int_sub  = int_binary_op ( - ) 
let int_mult = int_binary_op ( * ) 
let int_div  = int_binary_op ( / ) 
let int_succ = int_unary_op succ
let int_pred = int_unary_op pred
let int_to_str = unary_fun from_int Int.to_string to_str

(* TRUTH AND LIES *)
let bool_unary_op (op : (bool -> bool)) : out =
  unary_fun from_bool op to_bool
let bool_binary_op (op : (bool -> bool -> bool)) : out =
  binary_fun from_bool from_bool op to_bool
let bool_and = bool_binary_op (&&)
let bool_or  = bool_binary_op (||) 
let bool_not = bool_unary_op (not) 
let bool_to_str = unary_fun from_bool Bool.to_string to_str
let eq = 
  Fun (fun (o1 : out) -> Fun (fun (o2 : out) -> 
    let b_val =
      match (o1, o2) with
      | (Int a, Int b)   -> a = b
      | (Str a, Str b)   -> a = b
      | (Bool a, Bool b) -> a = b
      | _ -> false
    in Bool b_val))

(* STRING THINGS *)
let str_unary_op (op : (string -> string)) : out =
  unary_fun from_str op to_str
let str_binary_op (op : (string -> string -> string)) : out =
  binary_fun from_str from_str op to_str
let str_concat = str_binary_op (^)
let str_len = unary_fun from_str String.length to_int

(* THIS IS THE STUFF WE CAN DO NOW *)
let starting_env : env = [(Var "add", int_add); (Var "sub", int_sub);
                          (Var "mult", int_mult); (Var "div", int_div);
                          (Var "succ", int_succ); (Var "pred", int_pred);
                          (Var "int_to_str", int_to_str);
                          (Var "and", bool_and); (Var "or", bool_or);
                          (Var "not", bool_not); (Var "eq", eq);
                          (Var "bool_to_str", bool_to_str);
                          (Var "concat", str_concat); (Var "len", str_len);]

(* NOW THIS IS HOW TO MAKE THINGS HAPPEN *)
let run (exp : exp) : out =
  interp exp starting_env
