open Core.Std
open Lexing      (* GUY IN CHARGE OF WHEN THE LEXER GETS CHARS *)
open Lexer       (* GUY IN CHARGE OF INPUT->TOKENS *)
open Parser      (* GUY IN CHARGE OF TOKENS->AST *)
open Catbird     (* GUY IN CHARGE OF AST->OUTPUT *)

let lex_and_parse (input : string) : Catbird.exp option =
  Parser.parse Lexer.read (Lexing.from_string input)

let interp_ast (ast : Catbird.exp option) : string =
  match ast with
  | Some exp -> Catbird.result exp
  | None -> ""

let rec repl () : unit =
  print_newline ();
  print_string "> ";
  try
    read_line () 
    |> lex_and_parse
    |> interp_ast
    |> print_string;
    repl ()
  with
    End_of_file -> ()

let () = repl ()
