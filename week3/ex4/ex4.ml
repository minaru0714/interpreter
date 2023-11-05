open Ex4Syntax
open Ex4Parser
open Ex4Lexer
open Type

(* ファイルから式を読み込んで評価する関数 *)
let rec evaluate_from_file filename:unit =
  let channel = open_in filename in
  let lexbuf = Lexing.from_channel stdin in
  let rec loop():unit =
  try
    let result =  Ex4Parser.main  Ex4Lexer.token lexbuf in
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
      let cmd =  Ex4Parser.command  Ex4Lexer.token lexbuf in
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
        loop (new_ty_env, (var, value) :: eval_env)
  
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
        loop (new_ty_env, (f, VRecFun(f, x, exp, eval_env)) :: eval_env)
     
     | CRecFunand fs -> 
      let tuple_expr = ERecFunand(fs, ETuple(List.map(fun(f,_,_) -> EVar f)fs)) in   
      let (tuple_type, new_ty_env) = infer_expr ty_env tuple_expr in
      match tuple_type with
      | TTuple types when List.length types = List.length fs ->
      let new_type_env = List.fold_left2 (fun env (f,_,_) t -> (f, t) :: env ) new_ty_env fs types in
       (*let rec extend_env idx fs env = match fs with
        | [] -> env
        | (f, x, e') :: tail ->
          let env' = (f, VRecFunand (idx, fs, env)) :: env in
          extend_env (idx + 1) tail env'
          in
        let extended_env = extend_env 1 fs eval_env in*)
        let function_mappings = List.mapi (fun i (f, x, e) -> 
          (f, VRecFunand (i+1, fs, eval_env))) fs 
        in
        let eval_new_env = (function_mappings @ eval_env) in
      loop(new_type_env, eval_new_env)
      | _ -> failwith "Error"

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