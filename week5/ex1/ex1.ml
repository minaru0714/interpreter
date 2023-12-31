open LazySyntax
open Ex1Parser
open Ex1Lexer
open Type

(* ファイルから式を読み込んで評価する関数 *)
let rec evaluate_from_file filename:unit =
  let channel = open_in filename in
  let lexbuf = Lexing.from_channel stdin in
  let rec loop():unit =
  try
    let result =  Ex1Parser.main  Ex1Lexer.token lexbuf in
     print_value (eval [] result) ;  print_newline () ; (*改行*)
  loop ()
with
  | Parsing.Parse_error -> print_endline "Parse Error!" (*解析エラー*)
  | Eval_error -> print_endline "Eval Error!" (*評価エラー*)
in loop();
  close_in channel 

  let interactive_loop () =
    let lexbuf = Lexing.from_channel stdin in
    let rec loop (ty_env, eval_env)  =
    try
      let cmd =  Ex1Parser.command  Ex1Lexer.token lexbuf in
      match cmd with
      | CExp exp ->
        let (inferred_type, new_ty_env) = infer_expr ty_env exp in
        print_string "type: ";
        print_type inferred_type;
        print_newline ();
        let value = eval eval_env exp in
        print_value value;
        print_newline ();
        loop (new_ty_env, eval_env)
        
      | CLet (var, exp) ->
        let (inferred_type, new_ty_env) = infer_expr ty_env exp in
        print_string (var ^ " : ");
        print_type inferred_type;
        print_newline ();
        print_string var;
        print_string " = ";
        let value = eval eval_env exp in
        print_value value;
        print_newline ();
        loop (new_ty_env, (var, Thunk(EValue value, eval_env)) :: eval_env)

      | CRecFun (f, x, exp) as cmd -> 
      let (inferred_type, new_ty_env) = infer_cmd ty_env cmd in
      let print_ty_env env =
          List.iter (fun (var, t) -> 
              print_string (var ^ ": ");
              print_type t;
              print_newline ();
          ) env in
        print_ty_env inferred_type;
        print_newline ();
        loop (new_ty_env, (f, Thunk(EValue (VRecFun(f, x, exp, eval_env)), eval_env)) :: eval_env)
    
  
    with
      | Parsing.Parse_error -> print_endline "Parse Error!" (*解析エラー*)
      | Eval_error -> print_endline "Eval Error!" (*評価エラー*)
      | Unification_failure msg -> print_endline ("Type inference failed: " ^ msg)
      in loop ([], [])

let main () =
  match Array.length Sys.argv with
   1 -> interactive_loop()
  |x -> evaluate_from_file Sys.argv.(1)


let _ = main ()