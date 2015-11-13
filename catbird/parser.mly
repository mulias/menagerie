%{
open Catbird
open Core.Std

(* YUM CURRY *)
(* WE WANT
 * Lambda ([Var "a"; Var "b"], Int 4)
 * TO LOOK LIKE
 * Lambda (Var "a", Lambda (Var "b", Int 4))
 *)
let curry_lambda (vars : exp list) (body : exp) : exp =
  match List.rev vars with
  | []     -> Lambda (Const UE, body)
  | hd::tl -> List.fold tl ~init:(Lambda (hd, body))
                ~f:(fun inner_l next_var -> (Lambda (next_var, inner_l)))

(* WE WANT
 * Apply (Var "add", [Int 1; Int 2]) 
 * TO LOOK LIKE
 * Apply (Apply (Var "add", Int 1), Int 2) 
 *)
let curry_apply (f : exp) (args : exp list) : exp =
  List.fold args ~init:f ~f:(fun accum next_arg -> (Apply (accum, next_arg)))

%}
%token <int> INT
%token <string> STR
%token <string> VAR
%token TRUE
%token FALSE
%token QUOTE
%token LAMBDA
%token BLANK
%token OP
%token CP
%token EOF
%start parse
%type<Catbird.exp option> parse
%%

parse: 
  | EOF                                                { None                 }
  | e = exp                                            { Some e               };

exp:
  | x = VAR                                            { Var x                }
  | OP; LAMBDA; OP; BLANK; CP; b = exp; CP             { Lambda (Const UE, b) } 
  | OP; LAMBDA; OP; vs = list(l_vars); CP; b = exp; CP { curry_lambda vs b    } 
  | OP; f = exp; args = list(exp); CP                  { curry_apply f args   }
  | c = const                                          { Const c              };

l_vars:
  | x = VAR                                            { Var x                };

const:
  | i = INT                                            { Int i                }
  | s = STR                                            { Str s                }
  | TRUE                                               { Bool true            }
  | FALSE                                              { Bool false           }
  | QUOTE; s = VAR                                     { Sym s                }
  | QUOTE; OP; l = list(inside_list); CP               { List l               }
  | BLANK                                              { UE                   };

inside_list:
  | i = INT                                            { Int i               }
  | s = STR                                            { Str s               }
  | TRUE                                               { Bool true           }
  | FALSE                                              { Bool false          }
  | s = VAR                                            { Sym s               }
  | OP; l = list(inside_list); CP                      { List l              }
  | BLANK                                              { UE                  };
