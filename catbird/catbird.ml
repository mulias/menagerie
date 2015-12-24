open Core.Std

let lex_and_parse (input : string) : Exp.t option =
  Parser.parse Lexer.read (Lexing.from_string input)

let interp_ast (ast : Exp.t option) : string =
  match ast with
  | Some exp -> Interpreter.result exp
  | None -> ""

let evaluate (input : string) : string =
  interp_ast (lex_and_parse input)

let eval_file filename : unit =
  let ic = In_channel.create ~binary:true filename in
  print_string "file eval not yet supported"
	
let rec eval_repl () : unit =
  print_newline ();
  print_string "> ";
  try
    print_string (evaluate (read_line ()));
    eval_repl ()
  with
    End_of_file -> ()
	
let run_tests () : unit =
  print_string "tests not yet added"
  
let command =
  Command.basic
    ~summary:"test"
    Command.Spec.(empty +> anon (maybe ("filename" %: file)))
    (fun source () ->
       match source with
       | Some filename -> eval_file filename
       | None          -> eval_repl())

let () = 
  Command.run command; 
  print_newline ()
    
