(** 
parser.mly
Parsing rules to turn a stream of tokens into an s-expression.
**)

%{
open Sxp (* Llama module with s-expression data type *)
%}
%token <int> INT
%token <float> FLOAT
%token <string> STR
%token <string> SYM
%token <bool> BOOL
%token NULL
%token OPEN_PAREN
%token CLOSE_PAREN
%token EOF
%start main
%type<Sxp.t option> main
%%

main: (* empty or an s-expression *)
  | s = sxp                                     { Some s    }
  | EOF                                         { None      };

sxp: (* atom or a cons list of atoms *)
  | OPEN_PAREN; a = sxp; d = cdr; CLOSE_PAREN   { Cons(a,d) }
  | i = INT                                     { Int i     }
  | f = FLOAT                                   { Float f   }
  | s = STR                                     { Str s     }
  | x = SYM                                     { Sym x     }
  | b = BOOL                                    { Bool b    }
  | NULL                                        { Null      };

cdr: (* the cdr of a cons can either be a new cons or null *)
  | a = sxp; d = cdr                            { Cons(a,d) }
  | (* empty *)                                 { Null      };
