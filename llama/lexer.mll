(**
lexer.mll
Lexing rules to turn text input into a stream of tokens.
**)

{
open Parser (* Llama module describing tokens to parse to *)
open Lexing (* OCaml module to drive the lexer rules *)

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

(* token components *)
let digit = ['0'-'9']
let int_exp = ['e' 'E'] '+'? digit+
let float_exp = ['e' 'E'] ['-' '+']? digit+
let dec = '.' digit*
let char = ['a'-'z' 'A'-'Z']
let math = ['*' '-' '+' '/' '^' '%' '<' '>' '=']
let thingy = ['!' '@' '#' '$' '&' '_' '~' ':' '\\' '`']
let symbol = char | math | thingy
let any_valid = symbol | digit


(* token defs *)
let int = '-'? digit digit* int_exp?
let float = digit+ dec? float_exp? | dec digit+
let white = [' ' '\t']+
let newline  = '\r' | '\n' | "\r\n"
let sym = symbol any_valid*


rule read =
  parse
  | white     { read lexbuf }
  | newline   { next_line lexbuf; read lexbuf }
  | int       { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float     { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | "true"    { BOOL true }
  | "false"   { BOOL false }
  | "null"    { NULL }
  | sym       { SYM (Lexing.lexeme lexbuf) }
  | '"'       { read_string (Buffer.create 17) lexbuf }
  | '('       { OPEN_PAREN }
  | ')'       { CLOSE_PAREN }
  | _         { raise (SyntaxError("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof       { EOF }

and read_string buf =
  parse
  | '"'       { STR (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
  | _
    { raise (SyntaxError("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof
    { raise (SyntaxError("String is not terminated")) }
