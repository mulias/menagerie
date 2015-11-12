%{
open Catbird
open Core.Std

(* YUM CURRY! *)
(* WE WANT
 * Lambda ([Var "a"; Var "b"], Int 4)
 * TO LOOK LIKE
 * Lambda (Var "a", Lambda (Var "b", Int 4))
 *)
let curry_lambda (var_strs : string list) (body : exp) : exp =
  let vars = List.map var_strs ~f:(fun x -> Var x) in
  match List.rev vars with
  | []     -> Lambda (UE, body)
  | hd::tl -> List.fold tl ~init:(Lambda (hd, body))
                ~f:(fun inner_l next_var -> (Lambda (next_var, inner_l)))

(* WE WANT
 * Apply (Var "add", [Int 1; Int 2]) 
 * TO LOOK LIKE
 * Apply (Apply (Var "add", Int 1), Int 2) 
 *)
let curry_apply (f : exp) (args : exp list) : exp =
  match args with
  | []     -> (Apply (f, UE))
  | hd::tl -> List.fold tl ~init:(Apply (f, hd)) 
                ~f:(fun new_f next_arg -> (Apply (new_f, next_arg)))

%}
%token <int> INT
%token <string> STR
%token <string> VAR
%token TRUE
%token FALSE
%token QUOTE
%token LAMBDA
%token OP
%token CP
%token EOF
%start parse
%type<Catbird.exp option> parse
%%

parse: 
  | EOF                                                { None                }
  | e = exp                                            { Some e              };

exp:
  | x = VAR                                            { Var x               }
  | OP; LAMBDA; OP; vars = list(VAR); CP; b = exp; CP  { curry_lambda vars b } 
  | OP; f = exp; args = list(exp); CP                  { curry_apply f args  }
  | c = const                                          { Const c             };

const:
  | i = INT                                            { Int i               }
  | s = STR                                            { Str s               }
  | TRUE                                               { Bool true           }
  | FALSE                                              { Bool false          }
  | QUOTE; s = VAR                                     { Sym s               }
  | QUOTE; OP; l = list(const); CP                     { List l              };
