open Core.Std
open Val
open Monads.Or_failure

(* ENV *)
(* HELLO ASSOC, MY OLD FRIEND *)
(******************************************************************************)

type t = (Exp.t, Val.t) List.Assoc.t

(* BORING STARTING ENV *)
let empty_env : t = []


(* DEFAULT ENV *)
(* AWESOME, NOW WE STEAL BASIC FUNCTIONS FROM OCAML. *)
(******************************************************************************)

(* FIRST WE NEED SOME FUNCTION BUILDERS. *)

type 'a convm = ('a, string) Monads.Or_failure.t

let unary_fun (from_v : (Val.t -> 'a convm)) (op : ('a -> 'b)) 
  (to_v : ('b -> Val.t)) : Val.t =
  Fun (fun (v : Val.t) -> 
    (from_v v) >> (fun (a : 'a) -> 
    success (to_v (op a))))
    
let binary_fun (from_v1 : (Val.t -> 'a convm)) (from_v2 : (Val.t -> 'b convm)) 
  (op : ('a -> 'b -> 'c)) (to_v : ('c -> Val.t)) : Val.t =
  Fun (fun (v1 : Val.t) -> 
    (from_v1 v1) >> (fun (a : 'a) ->
    success (Fun (fun (v2 : Val.t) -> 
      (from_v2 v2) >> (fun (b : 'b) ->
      success (to_v (op a b)))))))

let from_int (v : Val.t) : int convm =
  match v with
  | Int i -> success i
  | _     -> failure "[arg type] expected integer"

let from_str (v : Val.t) : string convm =
  match v with
  | Str s -> success s
  | _     -> failure "[arg type] expected string"

let from_bool (v : Val.t) : bool convm =
  match v with
  | Bool b -> success b
  | _      -> failure "[arg type] expected boolean"

let from_list (v : Val.t) : (Val.t list) convm =
  match v with
  | List l -> success l
  | _      -> failure "[arg type] expected list"

let to_int (i : int) : Val.t = Int i

let to_str (s : string) : Val.t = Str s

let to_bool (b : bool) : Val.t = Bool b

let to_list (l : Val.t list) : Val.t = List l

(* MATHS *)
let int_unary_fun (op : (int -> int)) : Val.t =
  unary_fun from_int op to_int
    
let int_binary_fun (op : (int -> int -> int)) : Val.t =
  binary_fun from_int from_int op to_int

let add_f  = int_binary_fun ( + ) 
let sub_f  = int_binary_fun ( - ) 
let mult_f = int_binary_fun ( * ) 
let div_f  = int_binary_fun ( / ) 
let succ_f = int_unary_fun succ
let pred_f = int_unary_fun pred

(* TRUTH AND LIES *)
let bool_unary_fun (op : (bool -> bool)) : Val.t =
  unary_fun from_bool op to_bool

let bool_binary_fun (op : (bool -> bool -> bool)) : Val.t =
  binary_fun from_bool from_bool op to_bool

let and_f = bool_binary_fun (&&)
let or_f  = bool_binary_fun (||) 
let not_f = bool_unary_fun (not) 

let eq_f : Val.t = 
  Fun (fun (v1 : Val.t) -> 
  success (Fun (fun (v2 : Val.t) ->
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
    in success (to_bool result))))

(* STRING THINGS *)
let str_unary_fun (op : (string -> string)) : Val.t =
  unary_fun from_str op to_str

let str_binary_fun (op : (string -> string -> string)) : Val.t =
  binary_fun from_str from_str op to_str

let concat_f = str_binary_fun (^)
let len_f = unary_fun from_str String.length to_int
let to_str_f = Fun (fun (v : Val.t) -> success (to_str (Val.to_string v)))

(* LISTS WITH ALL THE CRYPTIC OPERATIONS *)
let cons_f : Val.t =  
  Fun (fun (v1 : Val.t) ->
  success (Fun (fun (v2 : Val.t) ->
    (from_list v2) >> (fun (l : Val.t list) ->
    success (to_list (v1 :: l))))))

let car_f : Val.t =
  Fun (fun (v : Val.t) ->
    (from_list v) >> (fun (l : Val.t list) ->
    match l with
    | hd::_ -> success hd
    | []    -> failure "[car] expected non-empty list"))

let cdr_f : Val.t =
  Fun (fun (v : Val.t) ->
    (from_list v) >> (fun (l : Val.t list) ->
    match l with
    | _::tl -> success (to_list tl)
    | []    -> failure "[cdr] expected non-empty list"))

(* THIS IS THE STUFF WE CAN DO NOW *)
let initial_env : t = [(Exp.Var "+", add_f); (Exp.Var "-", sub_f);
                       (Exp.Var "*", mult_f); (Exp.Var "/", div_f);
                       (Exp.Var "++", succ_f); (Exp.Var "--", pred_f);
                       (Exp.Var "&", and_f); (Exp.Var "|", or_f);
                       (Exp.Var "!", not_f); (Exp.Var "=", eq_f);
                       (Exp.Var "concat", concat_f); (Exp.Var "len", len_f);
                       (Exp.Var "->str", to_str_f);
                       (Exp.Var "#", cons_f); (Exp.Var "hd#", car_f); 
                       (Exp.Var "#tl", cdr_f);]