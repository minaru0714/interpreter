open LazySyntax
open Ex3Parser
open Ex3Lexer
open Type

(* ファイルから式を読み込んで評価する関数 *)
let rec evaluate_from_file filename:unit =
  let channel = open_in filename in
  let lexbuf = Lexing.from_channel stdin in
  let rec loop():unit =
  try
    let result =  Ex3Parser.main  Ex3Lexer.token lexbuf in
    print_value ((force_to_value (eval [] result))) ;  print_newline () ; (*改行*)
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
      let cmd =  Ex3Parser.command  Ex3Lexer.token lexbuf in
      match cmd with
      | CExp exp ->
        let (inferred_type, new_ty_env) = infer_expr ty_env exp in
        print_string "type: ";
        print_type inferred_type;
        print_newline (); 
        let value = eval eval_env exp in
        print_value (force_to_value value);
        print_newline ();
        loop (new_ty_env, eval_env)

      | CLet (var, exp) ->
        let (inferred_type, new_ty_env) = infer_expr ty_env exp in
        print_string (var ^ " : ");
        print_type inferred_type;
        print_newline ();
        loop ((var, inferred_type)::new_ty_env, (var, Thunk(exp, eval_env)) :: eval_env)

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

      | CRecFunand fs -> 
          let tuple_expr = ERecFunand(fs, ETuple(List.map(fun(f,_,_) -> EVar f)fs)) in   
          let (tuple_type, new_ty_env) = infer_expr ty_env tuple_expr in
          (match tuple_type with
          | TTuple types when List.length types = List.length fs ->
          let new_type_env = List.fold_left2 (fun env (f,_,_) t -> (f, t) :: env ) new_ty_env fs types in
            let function_mappings = List.mapi (fun i (f, x, e) -> 
            (f, Thunk(EValue(VRecFunand (i+1, fs, eval_env)), eval_env))) fs in
              let eval_new_env = (function_mappings @ eval_env) in
             loop(new_type_env, eval_new_env)
          | _ -> failwith "Error"
          )
          | CLetRec fs ->
          let inferred_types_with_envs = List.map (fun (var, exp) -> (var, infer_expr ty_env exp)) fs in
          let new_ty_env = List.fold_left (fun env (var, (ty, _)) -> (var, ty) :: env) ty_env inferred_types_with_envs in
          let new_eval_env = List.map (fun (var, exp) -> (var, Thunk(exp, eval_env))) fs in
          loop (new_ty_env, new_eval_env @ eval_env)
        


  
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