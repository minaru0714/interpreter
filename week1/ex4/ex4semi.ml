open Ex4Syntax
open Ex4Parser
open Ex4Lexer

(* ファイルから式を読み込んで評価する関数 *)
let rec evaluate_from_file filename:unit =
  let channel = open_in filename in
  let lexbuf = Lexing.from_channel stdin in
  let rec loop():unit =

    try
    let result = Ex4Parser.main Ex4Lexer.token lexbuf in
  print_value [] result ;  print_newline () ; (*改行*)
  loop ()
with
  | Parsing.Parse_error -> print_endline "Parse Error!" (*解析エラー*)
  | Eval_error -> print_endline "Eval Error!" (*評価エラー*)
in loop();
  close_in channel 



let  interactive_loop () =
    let lexbuf = Lexing.from_channel stdin in
    let rec loop():unit =
    try
      let result = Ex4Parser.main Ex4Lexer.token lexbuf in
    print_value [] result;  print_newline () ; (*改行*)
    loop ()
  with
    | Parsing.Parse_error -> print_endline "Parse Error!" (*解析エラー*)
    | Eval_error -> print_endline "Eval Error!" (*評価エラー*)
  in loop()


let main () =
  match Array.length Sys.argv with
   1 -> interactive_loop()
  |x -> evaluate_from_file Sys.argv.(1)


let _ = main ()