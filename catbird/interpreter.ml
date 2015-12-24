open Core.Std
open Exp
open Val
open Env
open Monads.Or_failure

(* INTERPRETER *)
(* INTERPRET AN EXP IN AN ENV, GET A VALUE OR STRING *)
(******************************************************************************)

let interp_const (const : Exp.const) : Val.tm =
  let rec kernel (const : Exp.const) : Val.t =
    match const with
    | Int i     -> Int i
    | Str s     -> Str s
    | Bool b    -> Bool b
    | Sym s     -> Sym s
    | List exps -> List (List.map exps kernel)
    | UE        -> UV
  in success (kernel const)

let rec interp (exp : Exp.t) (env : Env.t) : Val.tm =
  let interp_var (exp : Exp.t) : Val.tm =
    match List.Assoc.find env exp with
    | Some v -> success v
    | None   -> failure "[env lookup] unbound variable"
  in
  let interp_lambda (var : Exp.t) (body : Exp.t) : Val.tm =
    match var with
    | Var _    -> success (Fun (fun x -> (interp body ((var, x) :: env))))
    | Const UE -> success (Fun (fun _ -> (interp body env)))
    | _        -> failure "[lambda] arg is not a variable"
  in
  let interp_apply (lambda : Exp.t) (exp : Exp.t) : Val.tm =
    (interp exp env) >> (fun exp_res ->
    (interp lambda env) >> (fun lambda_res ->
      match lambda_res with
      | Fun f -> f exp_res
      | _     -> failure "[apply] arg is not a function"))
  in
  let interp_define (var : Exp.t) (exp : Exp.t) : Val.tm =
    (interp exp env) >> (fun _ ->
      match var with
      | Var _ -> success UV
      | _     -> failure "[define] cannot bind expression to non-variable arg")
  in
  let interp_let (var : Exp.t) (exp : Exp.t) (body : Exp.t) : Val.tm =
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

let run (exp : Exp.t) : Val.tm =
  interp exp initial_env

let result (exp : Exp.t) : string =
  Val.tm_to_string (run exp)
