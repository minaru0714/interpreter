
open Week2ex1Syntax
open Week2ex1Parser
open Week2ex1Lexer

(* ファイルから式を読み込んで評価する関数 *)
let rec evaluate_from_file filename:unit =
  let channel = open_in filename in
  let lexbuf = Lexing.from_channel stdin in
  let rec loop():unit =
  try
    let result =  Week2ex1Parser.main  Week2ex1Lexer.token lexbuf in
     print_value (eval [] result) ;  print_newline () ; (*改行*)
  loop ()
with
  | Parsing.Parse_error -> print_endline "Parse Error!" (*解析エラー*)
  | Eval_error -> print_endline "Eval Error!" (*評価エラー*)
in loop();
  close_in channel 



let  interactive_loop () =
    let lexbuf = Lexing.from_channel stdin in
    let rec loop env  =
    try
      let exp =  Week2ex1Parser.command  Week2ex1Lexer.token lexbuf in
      match exp with
      | CExp exp ->
        print_value  (eval env exp);
        print_newline ();
        loop env
      | CLet (var, exp) ->
        print_string var ;
        print_string " = ";
        let value = eval env exp in
        print_value  value;
        print_newline ();
        loop ((var, value) :: env)
  with
    | Parsing.Parse_error -> print_endline "Parse Error!" (*解析エラー*)
    | Eval_error -> print_endline "Eval Error!" (*評価エラー*)
  in loop []

let main () =
  match Array.length Sys.argv with
   1 -> interactive_loop()
  |x -> evaluate_from_file Sys.argv.(1)


let _ = main ()