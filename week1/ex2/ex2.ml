open Ex2Syntax
open Ex2Parser
open Ex2Lexer


let main () =
  try
    let lexbuf = Lexing.from_channel stdin in
    let result = Ex2Parser.main Ex2Lexer.token lexbuf in
    print_expr result; print_newline()
  with
    | Parsing.Parse_error -> print_endline "Parse Error!"
    | Eval_error -> print_endline "Eval Error!"
;;

if !Sys.interactive then
  ()
else
  main ()