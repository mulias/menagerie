open Core.Std


(* EXP *)
(* THE AST WE BUILD THEN INTERPRET *)

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


(* VALUE *)
(* OUTPUT WE WANT TO PRODUCE *)
type value =
  | Int of int
  | Str of string
  | Bool of bool
  | Sym of string
  | Fun of (value -> value)
  | List of value list
  | Error

let rec value_to_string (v : value) : string =
  match v with
  | Int i  -> Int.to_string i
  | Str s  -> "\"" ^ s ^ "\""
  | Bool b -> Bool.to_string b
  | Sym s  -> "'" ^ s
  | List l -> "'(" ^ (String.concat (List.map l value_to_string) ~sep:" ") ^ ")" 
  | Fun _ -> "<function>"
  | Error -> "Error"


(* ENV *)
(* HELLO ASSOC, MY OLD FRIEND *)
type env = (exp, value) List.Assoc.t

(* AWESOME, NOW WE STEAL BASIC FUNCTIONS FROM OCAML. THESE ARE MODULER BITS
 * TO HELP BUILD FUNCTIONS *)
let unary_fun (from_v : (value -> 'a option)) (f : ('a -> 'b))
              (to_v : ('b -> value)) : value =
  Fun (fun (v1 : value) -> 
    match from_v v1 with
    | Some a -> to_v (f a)
    | None -> Error)

let binary_fun (from_v1 : (value -> 'a option)) (from_v2 : (value -> 'b option))
               (f : ('a -> 'b -> 'c)) (to_v : ('c -> value)) : value =
  Fun (fun (v1 : value) -> Fun (fun (v2 : value) -> 
    match (from_v1 v1, from_v2 v2) with
    | (Some a, Some b) -> to_v (f a b)
    | (None, _) -> Error
    | (_, None) -> Error))

let from_int (v : value) : int option =
  match v with
  | Int a -> Some a
  | _ -> None

let from_str (v : value) : string option =
  match v with
  | Str s -> Some s
  | _ -> None

let from_bool (v : value) : bool option =
  match v with
  | Bool b -> Some b
  | _ -> None

let from_list (v : value) : value list option =
  match v with
  | List l -> Some l
  | _ -> None

let to_int (i : int) : value = Int i

let to_str (s : string) : value = Str s

let to_bool (b : bool) : value = Bool b

let to_list (l : value list) : value = List l

(* MATH *)
let int_unary_op (op : (int -> int)) : value =
  unary_fun from_int op to_int
let int_binary_op (op : (int -> int -> int)) : value =
  binary_fun from_int from_int op to_int
let add_f  = int_binary_op ( + ) 
let sub_f  = int_binary_op ( - ) 
let mult_f = int_binary_op ( * ) 
let div_f  = int_binary_op ( / ) 
let succ_f = int_unary_op succ
let pred_f = int_unary_op pred

(* TRUTH AND LIES *)
let bool_unary_op (op : (bool -> bool)) : value =
  unary_fun from_bool op to_bool
let bool_binary_op (op : (bool -> bool -> bool)) : value =
  binary_fun from_bool from_bool op to_bool
let and_f = bool_binary_op (&&)
let or_f  = bool_binary_op (||) 
let not_f = bool_unary_op (not) 
let eq_f = 
  Fun (fun (v1 : value) -> Fun (fun (v2 : value) -> 
    match (v1, v2) with
    | (Int a, Int b)   -> Bool (a = b)
    | (Str a, Str b)   -> Bool (a = b)
    | (Bool a, Bool b) -> Bool (a = b)
    | (Sym a, Sym b)   -> Bool (a = b)
    | (List a, List b) -> Bool (a = b)
    | (Error, _) | (_, Error) -> Error
    | (Int _, _)  | (Str _, _) | 
      (Bool _, _) | (Sym _, _) | 
      (Fun _, _)  | (List _, _) -> Bool (false)))

(* STRING THINGS *)
let str_unary_op (op : (string -> string)) : value =
  unary_fun from_str op to_str
let str_binary_op (op : (string -> string -> string)) : value =
  binary_fun from_str from_str op to_str
let concat_f = str_binary_op (^)
let len_f = unary_fun from_str String.length to_int
let to_str_f =
  Fun (fun (v1 : value) -> Str (value_to_string v1))

(* LISTS WITH ALL THE CRYPTIC OPERATIONS *)
let cons_f =  
  Fun (fun (v1 : value) -> Fun (fun (v2 : value) -> 
    match from_list v2 with
    | Some l -> List (v1::l)
    | None -> Error))
let car_f =
  Fun (fun (v1 : value) -> 
    match from_list v1 with
    | Some (hd::_) -> hd
    | Some [] -> Error
    | None -> Error)
let cdr_f =
  Fun (fun (v1 : value) -> 
    match from_list v1 with
    | Some (_::tl) -> List tl
    | Some [] -> Error
    | None -> Error)

(* THIS IS THE STUFF WE CAN DO NOW *)
let initial_env : env = [(Var "add", add_f); (Var "sub", sub_f);
                         (Var "mult", mult_f); (Var "div", div_f);
                         (Var "succ", succ_f); (Var "pred", pred_f);
                         (Var "and", and_f); (Var "or", or_f);
                         (Var "not", not_f); (Var "eq", eq_f);
                         (Var "concat", concat_f); (Var "len", len_f);
                         (Var "to_str", to_str_f);
                         (Var "cons", cons_f); (Var "car", car_f); 
                         (Var "cdr", cdr_f);]

(* OR IF YOU WANT TO BE BORING *)
let empty : env = []


(* RUN *)
(* INTERPRET AN EXP IN AN ENV, GET A VALUE OR STRING *)
let rec interp (exp : exp) (env : env) : value =
  let rec convert_const (const : const) : value =
    match const with
    | Int i     -> Int i
    | Str s     -> Str s
    | Bool b    -> Bool b
    | Sym s     -> Sym s
    | List exps -> List (List.map exps convert_const)
  in let lookup_env (exp : exp) : value =
    match List.Assoc.find env exp with
    | Some v -> v
    | _ -> Error
  in let extend_env (arg : exp) (body :exp) : value =
    match arg with
    | Var _ -> Fun (fun x -> (interp body ((arg, x) :: env)))
    | _ -> Error
  in let apply (lambda : exp) (exp : exp) : value =
    match interp lambda env with
    | Fun f -> f (interp exp env)
    | _ -> Error
  in match exp with
  | Const c             -> convert_const c
  | Var _               -> lookup_env exp
  | Lambda (arg, body)  -> extend_env arg body
  | Apply (lambda, exp) -> apply lambda exp


(* NOW THIS IS HOW TO MAKE THINGS HAPPEN *)

let run (exp : exp) : value =
  interp exp initial_env

let result (exp : exp) : string =
  value_to_string (run exp);

