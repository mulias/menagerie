%{
open Catbird
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
  | EOF                                         { None           }
  | e = exp                                     { Some e         };

exp:
  | x = VAR                                     { Var x          }
  | OP; LAMBDA; OP; a = exp; CP; b = exp; CP    { Lambda (a, b)  } 
  | OP; e1 = exp; e2 = exp; CP                  { Apply (e1, e2) }
  | c = const                                   { Const c        };

const:
  | i = INT                                     { Int i          }
  | s = STR                                     { Str s          }
  | TRUE                                        { Bool true      }
  | FALSE                                       { Bool false     }
  | QUOTE; s = VAR                              { Sym s          }
  | QUOTE; OP; l = list(const); CP              { List l         };
