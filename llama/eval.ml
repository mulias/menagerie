(**
eval.ml
Recursivly evaluate a llama program described as an s-expression, returning
a new s-expression as output.
**)

open Core.Std
open Sxp

(**
Exceptions
**)

(* Variable is not in the environment *)
exception Undefined_variable of string

(* Procedure has the wrong number of arguments *)
exception Arity_mismatch of string

(* Procedure requires arg of specific type *)
exception Invalid_arg_type of string

(* S-expression is not evaluable procedure *)
exception Expected_procedure of string

(**
Helpers 
**)

(* Get the contents of the address part of the register number *)
let car (s : Sxp.t) : Sxp.t =
  match s with
  | Cons(a,_) -> a
  | _ -> raise (Invalid_arg_type ("car: " ^ Sxp.to_type_string s))

(* Get the contents of the decrement part of the register number *)
let cdr (s : Sxp.t) : Sxp.t =
  match s with
  | Cons(_,d) -> d
  | _ -> raise (Invalid_arg_type ("cdr: " ^ Sxp.to_type_string s))

(* Construct a new cell with a car and cdr *)
let cons (s1 : Sxp.t) (s2 : Sxp.t) : Sxp.t =
  match s2 with
  | Cons _ | Null -> Cons(s1,s2)
  | _ -> raise (Invalid_arg_type ("cons: " ^ Sxp.to_type_string s2))

(* retrieve one expression, expect the remaining list to be null *)
let get_one_arg (s : Sxp.t) ~(f_name : string) : Sxp.t =
  match s with
  | Cons(a,Null) -> a
  | _ -> raise (Arity_mismatch (f_name ^ ": " ^ Sxp.to_string s)) 

(* retrieve two expressions, expect the remaining list to be null *)
let get_two_args (s : Sxp.t) ~(f_name : string) : Sxp.t * Sxp.t =
  match s with
  | Cons(a1,Cons(a2,Null)) -> (a1, a2)
  | _ -> raise (Arity_mismatch (f_name ^ ": " ^ Sxp.to_string s)) 

(* get one argument from the sxp and return it *)
let quote (s : Sxp.t) : Sxp.t =
  get_one_arg s ~f_name:"quote"

let is_atom (s : Sxp.t) : Sxp.t =
  match s with
  | Sym _ | Int _ | Float _ | Bool _ | Str _ -> Bool true
  | _                                        -> Bool false


(**
Environment 
The global env is shared and contians all s-expressions declared with 'define'
The local env is used in the scope of a 'let' statment
**)

module Env = struct

  type t = { 
    mutable global : Sxp.t String.Map.t; 
	        local  : Sxp.t String.Map.t;
  }

  (* Make a new environment *)
  let empty : t =
    { global = String.Map.empty; local = String.Map.empty }

  (* Check the local env, then the global env for a variable *)
  let find (env : t) (key : string) : Sxp.t =
    match String.Map.find env.local key with
    | Some x -> x
    | None   -> 
     (match String.Map.find env.global key with
      | Some x -> x
      | None   -> raise (Undefined_variable key))

  (* Add variable to the persistant global environment *)
  let update_global (env : t) (key : string) (data : Sxp.t) : unit =
    env.global <- String.Map.add env.global ~key:key ~data:data

  (* Add variable to the temporary local environment *)
  let add_local (env : t) (key : string) (data : Sxp.t) : t =
    { env with local = String.Map.add env.local ~key:key ~data:data }

end


(**
Evaluator
**)

let rec 

  (* Recursivly evaluate sxp by resolving core procedures 
     and expanding variables stored in the environment    *)
  ev_rec (env : Env.t) (s : Sxp.t) : Sxp.t =
    match s with
    | Sym a                        -> Env.find env a
    | Cons(Sym "quote",x)          -> quote x
    | Cons(Sym "atom?",x)          -> ev_is_atom env x
    | Cons(Sym "car",x)            -> ev_car env x
    | Cons(Sym "cdr",x)            -> ev_cdr env x
    | Cons(Sym "cons",x)           -> ev_cons env x
    | Cons(Sym "cond",x)           -> ev_cond env x
    | Cons(Sym "=?",x)             -> ev_is_eq env x
    | Cons(Sym ">?",x)             -> ev_is_greater env x
    | Cons(Sym "<?",x)             -> ev_is_lesser env x
    | Cons(Sym "+",x)              -> ev_add env x
    | Cons(Sym "-",x)              -> ev_sub env x
    | Cons(Sym "/",x)              -> ev_div env x
    | Cons(Sym "*",x)              -> ev_mult env x
    | Cons(Sym "%",x)              -> ev_mod env x
    | Cons(Sym sym,x)              -> ev_find env sym x
    | Cons(Cons(Sym "let",v),x)    -> ev_let env v x
    | Cons(Cons(Sym "lambda",f),x) -> ev_lambda env f x
    | Cons _                       -> raise (Expected_procedure 
	                                          ("got: "^Sxp.to_string s))
    | _                            -> s

and 
     
  (* Eval and execute is_atom *)
  ev_is_atom (env : Env.t) (s : Sxp.t) : Sxp.t =
    get_one_arg s ~f_name:"atom?" |> ev_rec env |> is_atom

and

  (* Eval and execute car *)
  ev_car (env : Env.t) (s : Sxp.t) : Sxp.t =
    ev_rec env s |> get_one_arg ~f_name:"car" |> car

and

  (* Eval and execute cdr *)
  ev_cdr (env : Env.t) (s : Sxp.t) : Sxp.t =
    get_one_arg s ~f_name:"cdr" |> ev_rec env |> cdr

and

  (* Eval and execute cons *)
  ev_cons (env : Env.t) (s :Sxp.t) : Sxp.t =
    let a1, a2 = get_two_args s ~f_name:"cons" in 
    cons (ev_rec env a1) (ev_rec env a2)

and 

  (* Recursivly check each clause of cond, eval
     the code related to the first true clause *)
  ev_cond (env : Env.t) (s : Sxp.t) : Sxp.t =
    match s with
    | Cons(Cons(test,res),next) ->
     (match ev_rec env test with
      | Bool false -> ev_cond env next
      | Bool true  -> ev_rec env res 
      | _ -> raise (Invalid_arg_type ("cond: " ^ Sxp.to_type_string s)))
    | _ -> raise (Arity_mismatch ("cond: " ^ Sxp.to_string s))

and

  (* Eval and execute =, works for all sxp types *)
  ev_is_eq (env : Env.t) (s : Sxp.t) : Sxp.t =
    let a1, a2 = get_two_args s ~f_name:"=?" in 
    Bool ((ev_rec env a1) = (ev_rec env a2))

and

  (* Eval and execute >, behavior when comparing 
     mismatched types and non-atoms is unreliable *)
  ev_is_greater (env : Env.t) (s : Sxp.t) : Sxp.t =
    let a1, a2 = get_two_args s ~f_name:">?" in
    Bool ((ev_rec env a1) > (ev_rec env a2))

and

  (* Eval and execute <, behavior when comparing 
     mismatched types and non-atoms is unreliable *)
  ev_is_lesser (env : Env.t) (s : Sxp.t) : Sxp.t =
    let a1, a2 = get_two_args s ~f_name:"<?" in
    Bool ((ev_rec env a1) < (ev_rec env a2))

and

  (* Generic builder for int and float math functions
     recursivly call eval, error if arity is not two 
     if an int and float are used, cast int to float *)
  ev_math (env : Env.t) (s :Sxp.t) ~f_name ~f_int ~f_float : Sxp.t =
    let (a1, a2) = get_two_args s ~f_name in
    match ((ev_rec env a1),(ev_rec env a2)) with
    | (Int x,Int y)     -> Int (f_int x y)
    | (Float x,Float y) -> Float (f_float x y)
    | (Int x,Float y)   -> Float (f_float (Float.of_int x) y)
    | (Float x,Int y)   -> Float (f_float x (Float.of_int y))
    | _                 -> raise (Invalid_arg_type f_name)

and

  (* Eval and execute + for numbers *)
  ev_add (env : Env.t) (s : Sxp.t) : Sxp.t =
    ev_math env s ~f_name:"+" ~f_int:(+) ~f_float:(+.) 

and

  (* Eval and execute - for numbers *)
  ev_sub (env : Env.t) (s : Sxp.t) : Sxp.t =
    ev_math env s ~f_name:"-" ~f_int:(-) ~f_float:(-.) 

and

  (* Eval and execute * for numbers *)
  ev_mult (env : Env.t) (s : Sxp.t) : Sxp.t =
    ev_math env s ~f_name:"*" ~f_int:( * ) ~f_float:( *. ) 

and

  (* Eval and execute / for numbers. 
     Division of two Ints produces an Int. *)
  ev_div (env : Env.t) (s : Sxp.t) : Sxp.t =
    ev_math env s ~f_name:"/" ~f_int:(/) ~f_float:(/.) 

and

  (* Eval and execute mod for two Ints *)
  ev_mod (env : Env.t) (s : Sxp.t) : Sxp.t =
    match get_two_args (ev_rec env s) ~f_name:"%" with
    | (Int x, Int y) -> Int ((%) x y)
    | _              -> raise (Invalid_arg_type "%")

and

  (* Find a value in the environment, then eval with arguments *)
  ev_find (env : Env.t) (v : string) (s : Sxp.t) : Sxp.t =
    cons (Env.find env v) s |> ev_rec env

and

  (* Add value to local environment, then eval remaining *)
  ev_let (env : Env.t) (v : Sxp.t) (s : Sxp.t) : Sxp.t =
    s
    
and

  (* *)
  ev_lambda (env : Env.t) (f : Sxp.t) (s : Sxp.t) : Sxp.t =
    s

let eval (s : Sxp.t) : Sxp.t =
  ev_rec Env.empty s
