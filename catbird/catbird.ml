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

type exp =
  | Const of const
  | Var of string
  | Lambda of exp * exp 
  | Apply of exp * exp

let rec const_to_string (const : const) : string =
  match const with
  | Int i  -> Int.to_string i
  | Str s  -> "\"" ^ s ^ "\""
  | Bool b -> Bool.to_string b
  | Sym s  -> "'" ^ s
  | List l -> "'(" ^ (String.concat (List.map l const_to_string) ~sep:" ") ^ ")" 

let rec exp_to_string (exp : exp) : string =
  match exp with
  | Const c          -> const_to_string c
  | Var x            -> x
  | Lambda (v, body) -> 
      "(\\ (" ^ (exp_to_string v) ^ ") " ^ (exp_to_string body) ^ ")"
  | Apply (f, arg)   -> 
      "(" ^ (exp_to_string f) ^ " " ^ (exp_to_string arg) ^ ")"



(* STATE MONAD *)
(* OCAML HAS SOME BUILT IN MONAD FUNCTOR OR SOMETHING BUT I CAN'T BE BOTHERED
 * TO FIGURE THAT OUT THANK YOU. *)
(******************************************************************************)

type 'a m =
  | Success of 'a
  | Error of string

let (>>=) (am : 'a m) (f : ('a -> 'b m)) : 'b m =
  match am with
  | Success a -> f a
  | Error _   -> am



(* VALUE *)
(* OUTPUT WE WANT TO PRODUCE *)
(******************************************************************************)

type value =
  | Int of int
  | Str of string
  | Bool of bool
  | Sym of string
  | Fun of (value -> value m)
  | List of value list

let rec value_to_string (v : value) : string =
  match v with
  | Int i  -> Int.to_string i
  | Str s  -> "\"" ^ s ^ "\""
  | Bool b -> Bool.to_string b
  | Sym s  -> "'" ^ s
  | List l -> "'(" ^ (String.concat (List.map l value_to_string) ~sep:" ") ^ ")" 
  | Fun _ -> "<function>"

(* MORE MONAD. MONAD MONAD MONAD. DON'T I SOUND SMART. *)
type mvalue = value m

let mvalue_to_string (mvalue : mvalue) : string =
  match mvalue with
  | Success value -> value_to_string value
  | Error s       -> "Error: " ^ s



(* ENV *)
(* HELLO ASSOC, MY OLD FRIEND *)
(******************************************************************************)

type env = (exp, value) List.Assoc.t

(* AWESOME, NOW WE STEAL BASIC FUNCTIONS FROM OCAML. *)
let unary_fun (f : (value -> mvalue)) : value =
  Fun (fun (v1 : value) -> f v1)

let build_unary_fun (from_v : (value -> 'a m)) (f : ('a -> 'b))
                    (to_v : ('b -> value)) : value =
  unary_fun (fun (v : value) -> 
    match from_v v with
    | Success a -> Success (to_v (f a))
    | Error e -> Error e)

let binary_fun (f : (value -> value -> mvalue)) : value =
  Fun (fun (v1 : value) -> 
  Success (Fun (fun (v2 : value) -> f v1 v2)))

let build_binary_fun (from_v1 : (value -> 'a m)) (from_v2 : (value -> 'b m))
                     (f : ('a -> 'b -> 'c)) (to_v : ('c -> value)) : value =
  binary_fun (fun (v1 : value) (v2 : value) -> 
    match (from_v1 v1, from_v2 v2) with
    | (Success a, Success b) -> Success (to_v (f a b))
    | (Error e, _) -> Error e
    | (_, Error e) -> Error e)

let ternary_fun (f : (value -> value -> value -> mvalue)) : value =
  Fun (fun (v1 : value) -> 
  Success (Fun (fun (v2 : value) -> 
  Success (Fun (fun (v3 : value) -> f v1 v2 v3)))))

let from_int (v : value) : int m =
  match v with
  | Int a -> Success a
  | _     -> Error "[arg type] expected integer"

let from_str (v : value) : string m =
  match v with
  | Str s -> Success s
  | _     -> Error "[arg type] expected string"

let from_bool (v : value) : bool m =
  match v with
  | Bool b -> Success b
  | _      -> Error "[arg type] expected boolean"

let from_list (v : value) : (value list) m =
  match v with
  | List l -> Success l
  | _      -> Error "[arg type] expected list"

let to_int (i : int) : value = Int i

let to_str (s : string) : value = Str s

let to_bool (b : bool) : value = Bool b

let to_list (l : value list) : value = List l

(* MATH *)
let int_unary_op (op : (int -> int)) : value =
  build_unary_fun from_int op to_int

let int_binary_op (op : (int -> int -> int)) : value =
  build_binary_fun from_int from_int op to_int

let add_f  = int_binary_op ( + ) 
let sub_f  = int_binary_op ( - ) 
let mult_f = int_binary_op ( * ) 
let div_f  = int_binary_op ( / ) 
let succ_f = int_unary_op succ
let pred_f = int_unary_op pred

(* TRUTH AND LIES *)
let bool_unary_op (op : (bool -> bool)) : value =
  build_unary_fun from_bool op to_bool

let bool_binary_op (op : (bool -> bool -> bool)) : value =
  build_binary_fun from_bool from_bool op to_bool

let and_f = bool_binary_op (&&)
let or_f  = bool_binary_op (||) 
let not_f = bool_unary_op (not) 

let eq_f = 
  binary_fun (fun (v1 : value) (v2 : value) -> 
    match (v1, v2) with
    | (Int a, Int b)    -> Success (Bool (a = b))
    | (Str a, Str b)  -> Success (Bool (a = b))
    | (Bool a, Bool b)  -> Success (Bool (a = b))
    | (Sym a, Sym b)  -> Success (Bool (a = b))
    | (List a, List b) -> Success (Bool (a = b))
    | (Int _, _)  | (Str _, _)  | (Bool _, _) | 
      (Sym _, _)  | (Fun _, _)  | (List _, _)   -> Success (Bool (false)))

(* STRING THINGS *)
let str_unary_op (op : (string -> string)) : value =
  build_unary_fun from_str op to_str

let str_binary_op (op : (string -> string -> string)) : value =
  build_binary_fun from_str from_str op to_str

let concat_f = str_binary_op (^)
let len_f = build_unary_fun from_str String.length to_int
let to_str_f = unary_fun (fun (v : value) -> Success (Str (value_to_string v)))

(* LISTS WITH ALL THE CRYPTIC OPERATIONS *)
let cons_f =  
  binary_fun (fun (v1 : value) (v2 : value) -> 
    match v2 with
    | List l -> Success (List (v2 :: l))
    | _      -> Error "[arg type] expected list")

let car_f : value =
  let car (v : value) : mvalue =
    match v with
    | List (hd::_) -> Success hd
    | List []      -> Error "[car] expected non-empty list"
    | _            -> Error "[arg type] expected list"
  in Fun car

let cdr_f =
  let cdr (v : value) : mvalue =
    match v with
    | List (_::tl) -> Success (List tl)
    | List []      -> Error "[cdr] expected non-empty list"
    | _            -> Error "[arg type] expected list"
  in Fun cdr

(* THIS IS THE STUFF WE CAN DO NOW *)
let initial_env : env = [(Var "add", add_f); (Var "sub", sub_f);
                         (Var "mult", mult_f); (Var "div", div_f);
                         (Var "succ", succ_f); (Var "pred", pred_f);
                         (Var "and", and_f); (Var "or", or_f);
                         (Var "not", not_f); (Var "eq", eq_f);
                         (Var "concat", concat_f); (Var "len", len_f);
                         (Var "to_str", to_str_f);
                         (* (Var "cons", cons_f); *) (Var "car", car_f); 
                         (Var "cdr", cdr_f);]

(* OR IF YOU WANT TO BE BORING *)
let empty_env : env = []



(* RUN *)
(* INTERPRET AN EXP IN AN ENV, GET A VALUE OR STRING *)
(******************************************************************************)

let interp_const (const : const) : mvalue =
  let rec kernel (const : const) : value =
    match const with
    | Int i     -> Int i
    | Str s     -> Str s
    | Bool b    -> Bool b
    | Sym s     -> Sym s
    | List exps -> List (List.map exps kernel)
  in Success (kernel const)

let rec interp (exp : exp) (env : env) : mvalue =
  let interp_var (exp : exp) : mvalue =
    match List.Assoc.find env exp with
    | Some value -> Success value
    | _          -> Error "[env lookup] unbound variable"
  in
  let interp_lambda (arg : exp) (body :exp) : mvalue =
    match arg with
    | Var _ -> Success (Fun (fun x -> (interp body ((arg, x) :: env))))
    | _     -> Error "[lambda] arg is not a variable"
  in
  let interp_apply (lambda : exp) (exp : exp) : mvalue =
    (interp exp env)
    >>= (fun exp_res ->
      (interp lambda env) 
      >>= (fun lambda_res ->
        match lambda_res with
        | Fun f -> f exp_res
        | _     -> Error "[apply] not a function"))
  in
  match exp with
  | Const c             -> interp_const c
  | Var _               -> interp_var exp
  | Lambda (arg, body)  -> interp_lambda arg body
  | Apply (lambda, exp) -> interp_apply lambda exp

(* NOW THIS IS HOW TO MAKE THINGS HAPPEN *)

let run (exp : exp) : mvalue =
  interp exp initial_env

let result (exp : exp) : string =
  mvalue_to_string (run exp)
