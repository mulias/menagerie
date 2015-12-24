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

type exp =
  | Const of const
  | Var of string
  | Lambda of exp * exp 
  | Apply of exp * exp
  | Define of exp * exp
  | Let of exp * exp * exp

let rec const_to_string (const : const) : string =
  match const with
  | Int i  -> "Int "  ^ Int.to_string i 
  | Str s  -> "Str "  ^ "\"" ^ s ^ "\""
  | Bool b -> "Bool " ^ Bool.to_string b
  | Sym s  -> "Sym "  ^ "\"" ^ s ^ "\""
  | UE     -> "unit"
  | List l -> "List [" ^ (String.concat (List.map l const_to_string) ~sep:";") ^ "]"  

let rec exp_to_string (exp : exp) : string =
  match exp with
  | Const c -> "Const (" ^ (const_to_string c) ^ ")"
  | Var x   -> "Var " ^ "\"" ^ x ^ "\""
  | Lambda (v, body) -> 
      "Lambda (" ^ (exp_to_string v) ^ ", " ^ (exp_to_string body) ^ ")"
  | Apply (f, arg) -> 
      "Apply (" ^ (exp_to_string f) ^ ", " ^ (exp_to_string arg) ^ ")"
  | Define (v, e) ->
      "Define (" ^ (exp_to_string v) ^ ", " ^ (exp_to_string e) ^ ")"
  | Let (v, e, body) ->
      "Let (" ^ (exp_to_string v) ^ ", " ^ (exp_to_string e) ^ ", "
        ^ (exp_to_string body) ^ ")"


(* ERROR AND STATE MONAD *)
(* CORE HAS SOME BUILT IN MONAD FUNCTOR OR SOMETHING BUT I CAN'T BE BOTHERED
 * TO FIGURE THAT OUT THANK YOU. *)
(******************************************************************************)

(* ERROR MONAD. THIS ONE IS SIMPLE. *)
type ('a, 'e) or_failure =
  | Success of 'a
  | Failure of 'e

(* MONAD IS A SUCCESS, BUT DON'T LET THAT GET TO ITS HEAD *)
let success (a : 'a) : ('a, 'e) or_failure = Success a

(* MONAD IS A FAILURE, BUT DON'T BREAK IT'S HEART *)
let failure (e : 'e) : ('a, 'e) or_failure = Failure e

(* ERROR MONAD BIND. *)
let (>>) (m : ('a, 'e) or_failure) (f : ('a -> ('b, 'e) or_failure)) : ('b, 'e) or_failure =
  match m with
  | Success a -> f a
  | Failure e -> failure e

(*
(* STATE MONAD. THE TYPE OF A STATE MONAD IS A FUNCTION THAT TAKES A STATE AS
 * INPUT SO FUNCTIONS THAT RETURN STATE MONADS RETURN FUNCTIONS. YAY. *)
type ('a, 's) state_monad = 's -> ('a * 's)

let state_m (a : 'a) : state_monad = fun s -> (a, s)

(* STATE MONAD BIND. *)
let (>>S) (m : 'a state_monad) (f : 'a -> 'b state_monad) : 'b state_monad =
  fun s -> let (a, s1) = (m s) in ((f a) s2)
*)




(* VALUE *)
(* OUTPUT WE WANT TO PRODUCE *)
(******************************************************************************)

type value =
  | Int of int
  | Str of string
  | Bool of bool
  | Sym of string
  | Fun of (value -> mvalue)
  | List of value list
  | UV
and
mvalue = (value, string) or_failure

let rec value_to_string (v : value) : string =
  match v with
  | Int i  -> Int.to_string i
  | Str s  -> "\"" ^ s ^ "\""
  | Bool b -> Bool.to_string b
  | Sym s  -> "'" ^ s
  | List l -> "'(" ^ (String.concat (List.map l value_to_string) ~sep:" ") ^ ")" 
  | Fun _  -> "<function>"
  | UV     -> "unit"

let mvalue_to_string (mvalue : mvalue) : string =
  match mvalue with
  | Success v -> value_to_string v
  | Failure e -> "Error: " ^ e



(* ENV *)
(* HELLO ASSOC, MY OLD FRIEND *)
(******************************************************************************)

type env = (exp, value) List.Assoc.t

(* BORING STARTING ENV *)
let empty_env : env = []



(* DEFAULT ENV *)
(* AWESOME, NOW WE STEAL BASIC FUNCTIONS FROM OCAML. *)
(******************************************************************************)

(* FIRST WE NEED SOME FUNCTION BUILDERS. *)

type 'a mconverted = ('a, string) or_failure

let unary_fun (from_v : (value -> 'a mconverted)) 
              (op : ('a -> 'b)) 
              (to_v : ('b -> value)) : value =
  Fun (fun (v : value) -> 
    (from_v v) >> (fun (a : 'a) -> 
    success (to_v (op a))))
    
let binary_fun (from_v1 : (value -> 'a mconverted)) 
               (from_v2 : (value -> 'b mconverted)) 
               (op : ('a -> 'b -> 'c)) 
               (to_v : ('c -> value)) : value =
  Fun (fun (v1 : value) -> 
    (from_v1 v1) >> (fun (a : 'a) ->
    success (Fun (fun (v2 : value) -> 
      (from_v2 v2) >> (fun (b : 'b) ->
      success (to_v (op a b)))))))

let from_int (v : value) : int mconverted =
  match v with
  | Int i -> success i
  | _     -> failure "[arg type] expected integer"

let from_str (v : value) : string mconverted =
  match v with
  | Str s -> success s
  | _     -> failure "[arg type] expected string"

let from_bool (v : value) : bool mconverted =
  match v with
  | Bool b -> success b
  | _      -> failure "[arg type] expected boolean"

let from_list (v : value) : (value list) mconverted =
  match v with
  | List l -> success l
  | _      -> failure "[arg type] expected list"

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

let eq_f : value = 
  Fun (fun (v1 : value) -> 
  success (Fun (fun (v2 : value) ->
    let result =
      match (v1, v2) with
      | (Int a, Int b)   -> a = b
      | (Str a, Str b)   -> a = b
      | (Bool a, Bool b) -> a = b
      | (Sym a, Sym b)   -> a = b
      | (List a, List b) -> a = b
      | (UV , UV)        -> true
      | (Int _, _)  | (Str _, _)  | 
        (Bool _, _) | (Sym _, _)  | 
        (Fun _, _)  | (List _, _) | (UV, _)  -> false
    in success (Bool result))))

(* STRING THINGS *)
let str_unary_op (op : (string -> string)) : value =
  unary_fun from_str op to_str

let str_binary_op (op : (string -> string -> string)) : value =
  binary_fun from_str from_str op to_str

let concat_f = str_binary_op (^)
let len_f = unary_fun from_str String.length to_int
let to_str_f = Fun (fun (v : value) -> success (Str (value_to_string v)))

(* LISTS WITH ALL THE CRYPTIC OPERATIONS *)
let cons_f : value =  
  Fun (fun (v1 : value) ->
  success (Fun (fun (v2 : value) ->
    (from_list v2) >> (fun (l : value list) ->
    success (List (v1 :: l))))))

let car_f : value =
  Fun (fun (v : value) ->
    (from_list v) >> (fun (l : value list) ->
    match l with
    | hd::_ -> success hd
    | []    -> failure "[car] expected non-empty list"))

let cdr_f : value =
  Fun (fun (v : value) ->
    (from_list v) >> (fun (l : value list) ->
    match l with
    | _::tl -> success (List tl)
    | []    -> failure "[cdr] expected non-empty list"))

(* THIS IS THE STUFF WE CAN DO NOW *)
let initial_env : env = [(Var "+", add_f); (Var "-", sub_f);
                         (Var "*", mult_f); (Var "/", div_f);
                         (Var "++", succ_f); (Var "--", pred_f);
                         (Var "&", and_f); (Var "|", or_f);
                         (Var "!", not_f); (Var "=", eq_f);
                         (Var "concat", concat_f); (Var "len", len_f);
                         (Var "->str", to_str_f);
                         (Var "#", cons_f); (Var "hd#", car_f); 
                         (Var "#tl", cdr_f);]



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
    | UE        -> UV
  in success (kernel const)

let rec interp (exp : exp) (env : env) : mvalue =
  let interp_var (exp : exp) : mvalue =
    match List.Assoc.find env exp with
    | Some value -> success value
    | None       -> failure "[env lookup] unbound variable"
  in
  let interp_lambda (var : exp) (body : exp) : mvalue =
    match var with
    | Var _    -> success (Fun (fun x -> (interp body ((var, x) :: env))))
    | Const UE -> success (Fun (fun x -> (interp body env)))
    | _        -> failure "[lambda] arg is not a variable"
  in
  let interp_apply (lambda : exp) (exp : exp) : mvalue =
    (interp exp env) >> (fun exp_res ->
    (interp lambda env) >> (fun lambda_res ->
      match lambda_res with
      | Fun f -> f exp_res
      | _     -> failure "[apply] arg is not a function"))
  in
  let interp_define (var : exp) (exp : exp) : mvalue =
    (interp exp env) >> (fun exp_res ->
      match var with
      | Var _ -> success UV
      | _     -> failure "[define] cannot bind expression to non-variable arg")
  in
  let interp_let (var : exp) (exp : exp) (body : exp) : mvalue =
    (interp exp env) >> (fun exp_res ->
      match var with
      | Var _ -> interp body ((var, exp_res) :: env)
      | _     -> failure "[let] cannot bind expression to non-variable arg")
  in
  match exp with
  | Const c              -> interp_const c
  | Var _                -> interp_var exp
  | Lambda (arg, body)   -> interp_lambda arg body
  | Apply (lambda, exp)  -> interp_apply lambda exp
  | Define (var, exp)    -> interp_define var exp
  | Let (var, exp, body) -> interp_let var exp body

(* NOW THIS IS HOW TO MAKE THINGS HAPPEN *)

let run (exp : exp) : mvalue =
  interp exp initial_env

let result (exp : exp) : string =
  mvalue_to_string (run exp)
