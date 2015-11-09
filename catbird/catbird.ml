open Core.Std

(* AST AND OUTPUT TYPES *)
type const =
  | Int of int
  | Str of string
  | Bool of bool
  | Sym of string
  | List of const list

type exp =
  | Const of const
  | Var of string
  | Lambda of exp * exp
  | Apply of exp * exp
  | If of exp * exp * exp

type out =
  | Int of int
  | Str of string
  | Bool of bool
  | Sym of string
  | Fun of (out -> out)
  | List of out list
  | Error

(* HELLO ASSOC, MY OLD FRIEND *)

type env = (exp, out) List.Assoc.t

(* HOW TO INTERPRET AN EXP IN AN ENV *)

let rec interp (exp : exp) (env : env) : out =
  let rec convert_const (const : const) : out =
    match const with
    | Int i     -> Int i
    | Str s     -> Str s
    | Bool b    -> Bool b
    | Sym s     -> Sym s
    | List exps -> List (List.map exps convert_const)
  in let lookup_env (exp : exp) : out =
    match List.Assoc.find env exp with
    | Some out -> out
    | _ -> Error
  in let extend_env (arg : exp) (body :exp) : out =
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
  | Const c             -> convert_const c
  | Var _               -> lookup_env exp
  | Lambda (arg, body)  -> extend_env arg body
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

let from_list (out : out) : out list option =
  match out with
  | List l -> Some l
  | _ -> None

let to_int (i : int) : out = Int i

let to_str (s : string) : out = Str s

let to_bool (b : bool) : out = Bool b

let to_list (l : out list) : out = List l

(* MATH *)
let int_unary_op (op : (int -> int)) : out =
  unary_fun from_int op to_int
let int_binary_op (op : (int -> int -> int)) : out =
  binary_fun from_int from_int op to_int
let add_f  = int_binary_op ( + ) 
let sub_f  = int_binary_op ( - ) 
let mult_f = int_binary_op ( * ) 
let div_f  = int_binary_op ( / ) 
let succ_f = int_unary_op succ
let pred_f = int_unary_op pred

(* TRUTH AND LIES *)
let bool_unary_op (op : (bool -> bool)) : out =
  unary_fun from_bool op to_bool
let bool_binary_op (op : (bool -> bool -> bool)) : out =
  binary_fun from_bool from_bool op to_bool
let and_f = bool_binary_op (&&)
let or_f  = bool_binary_op (||) 
let not_f = bool_unary_op (not) 
let eq_f = 
  Fun (fun (o1 : out) -> Fun (fun (o2 : out) -> 
    match (o1, o2) with
    | (Int a, Int b)   -> Bool (a = b)
    | (Str a, Str b)   -> Bool (a = b)
    | (Bool a, Bool b) -> Bool (a = b)
    | (Sym a, Sym b)   -> Bool (a = b)
    | (List a, List b) -> Bool (a = b)
    | (Int _, _)  | (Str _, _) | 
      (Bool _, _) | (Sym _, _) | 
      (Fun _, _)  | (List _, _) -> Bool (false)
    | (Error, _) | (_, Error) -> Error))

(* STRING THINGS *)
let str_unary_op (op : (string -> string)) : out =
  unary_fun from_str op to_str
let str_binary_op (op : (string -> string -> string)) : out =
  binary_fun from_str from_str op to_str
let concat_f = str_binary_op (^)
let len_f = unary_fun from_str String.length to_int
let rec rec_to_str (o : out) : string =
    match o with
    | Int i  -> Int.to_string i
    | Str s  -> "\"" ^ s ^ "\""
    | Bool b -> Bool.to_string b
    | Sym s  -> "'" ^ s
    | List l -> "'(" ^ (String.concat (List.map l rec_to_str) ~sep:" ") ^ ")" 
    | Fun _ -> "<function>"
    | Error -> "Error"
let to_str_f =
  Fun (fun (o1 : out) -> Str (rec_to_str o1))


(* LISTS WITH ALL THE CRYPTIC OPERATIONS *)
let cons_f =  
  Fun (fun (o1 : out) -> Fun (fun (o2 : out) -> 
    match from_list o2 with
    | Some l -> List (o1::l)
    | None -> Error))
let car_f =
  Fun (fun (o1 : out) -> 
    match from_list o1 with
    | Some (hd::tl) -> hd
    | Some [] -> Error
    | None -> Error)
let cdr_f =
  Fun (fun (o1 : out) -> 
    match from_list o1 with
    | Some (hd::tl) -> List tl
    | Some [] -> Error
    | None -> Error)

(* THIS IS THE STUFF WE CAN DO NOW *)
let starting_env : env = [(Var "add", add_f); (Var "sub", sub_f);
                          (Var "mult", mult_f); (Var "div", div_f);
                          (Var "succ", succ_f); (Var "pred", pred_f);
                          (Var "and", and_f); (Var "or", or_f);
                          (Var "not", not_f); (Var "eq", eq_f);
                          (Var "concat", concat_f); (Var "len", len_f);
                          (Var "to_str", to_str_f);
                          (Var "cons", cons_f); (Var "car", car_f); 
                          (Var "cdr", cdr_f);]

(* NOW THIS IS HOW TO MAKE THINGS HAPPEN *)
let show (o : out) : unit =
  print_string (rec_to_str o);
  print_newline ()

let run (exp : exp) : unit =
  show (interp exp starting_env)
