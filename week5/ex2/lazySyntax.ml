
type name = string

type binOp = OpAdd | OpSub | OpMul | OpDiv 

type pattern = PInt of int
             | PBool of bool
             | PVar of name
             | PNil
             | PCons of pattern * pattern
             | PTuple of pattern list

type expr = EValue of value  
          | EBin of binOp * expr * expr 
          | EEqual of expr * expr
          | ECompare of expr * expr
          | EIf of expr * expr * expr
          | EVar of name
          | ELet of name * expr * expr
          | EFun of name * expr
          | EApp of expr * expr
          | ERecFun of  name * name * expr * expr
          | EMatch of expr * (pattern * expr) list
          | ECons of expr * expr          (* e1 :: e2 *)
          | ETuple of expr list           (* (e1, e2, ..., en) *)
          | ERecFunand of (name * name * expr) list * expr
and env = (name * thunk) list
and value = VInt of int
          | VBool of bool
          | VFun of name * expr * env
          | VRecFun of name * name * expr * env
          | VNil
          | VCons of value * value
          | VTuple of value list
          | VRecFunand of int * (name * name * expr) list * env
and thunk = Thunk of expr * env



type command = CExp of expr
             | CLet of name * expr
             | CRecFun of name * name * expr


exception Eval_error

let rec find_match : pattern -> value -> env option = fun p v ->
  match p, v with
  | PInt n, VInt m -> if n = m then Some [] else None
  | PBool b1, VBool b2 -> if b1 = b2 then Some [] else None
  | PVar x, _ -> Some [(x, Thunk (EValue v, []))]
  | PNil, VNil -> Some []
  | PCons(p1, p2), VCons(v1, v2) ->
    (match find_match p1 v1, find_match p2 v2 with
    | Some env1, Some env2 -> Some (env1 @ env2)
    | _, _ -> None)
  | PTuple ps, VTuple vs ->
      (try
        let options = List.map2 find_match ps vs in
        let env_option = List.fold_left (fun acc_opt env_opt ->
          match acc_opt, env_opt with
          | Some acc, Some env -> Some (acc @ env)
          | _, _ -> None
        ) (Some []) options in
        env_option
      with Invalid_argument _ -> None)
  | _, _ -> None

let rec force (Thunk (e, env)) = eval env e

and eval : env -> expr -> value = fun env expr ->
  match expr with
  | EValue (VInt i) -> VInt i
  | EValue (VBool b) -> VBool b
  | EValue _ -> raise Eval_error 
  | EBin (b, e1, e2) ->
  (match eval env e1 with
    | VBool _ -> raise Eval_error
    | VInt x ->
      (match eval env e2 with
       | VBool _ -> raise Eval_error
       | VInt y ->
         (match b with
          | OpAdd -> VInt (x + y)
          | OpSub -> VInt (x - y)
          | OpMul -> VInt (x * y)
          | OpDiv -> VInt (x / y)
         )
       | _ -> raise Eval_error
      )
    | _ -> raise Eval_error  
  )
  
| EEqual (e1, e2) ->
  (match eval env e1 with
    | VBool _ -> raise Eval_error
    | VInt x -> 
      (match eval env e2 with
       | VBool _ -> raise Eval_error
       | VInt y -> if x = y then VBool true else VBool false
       | _ -> raise Eval_error  
      )
    | _ -> raise Eval_error  )
  
| ECompare (e1, e2) ->
  (match eval env e1 with
    | VBool _ -> raise Eval_error
    | VInt x -> 
      (match eval env e2 with
       | VBool _ -> raise Eval_error
       | VInt y -> if x < y then VBool true else VBool false
       | _ -> raise Eval_error  
      )
    | _ -> raise Eval_error  
  )
  
| EIf (e, e1, e2) ->
  (match eval env e with
    | VInt _ -> raise Eval_error
    | VBool true -> eval env e1
    | VBool false -> eval env e2
    | _ -> raise Eval_error  
  )
| EVar x -> 
    let thk = List.assoc x env in
    force thk

| ELet (x, e1, e2) ->
    let v1 = eval env e1 in
    eval ((x, Thunk (EValue v1, [])) :: env) e2

| EFun (x, e) ->
    VFun (x, e, env)
    
| EApp (e1, e2) ->
    (match eval env e1 with
    | VFun (x, e, oenv) -> 
        let thk = Thunk (e2, env) in  
        eval ((x, thk) :: oenv) e
    | VRecFun(f, x, e, oenv) ->
        let env' = (x, Thunk (e2, env)) :: (f, Thunk (EValue (VRecFun (f, x, e, oenv)), oenv)) :: oenv in 
        eval env' e
    | VRecFunand (idx, fns, oenv) ->
        let (f, x, e) = List.nth fns (idx - 1) in
        let env' = (x, Thunk (e2, env)) :: (List.mapi (fun i (f, x, e) -> 
            (f, Thunk (EValue (VRecFunand (i+1, fns, oenv)), oenv))) fns) @ oenv in
        eval env' e
    | _ -> raise Eval_error)

 | ERecFun (f, x, e1, e2) ->
    let env' = (f, Thunk (EValue (VRecFun(f, x, e1, env)), env)) :: env in
    eval env' e2

 | EMatch (e, cases) -> 
        let v = eval env e in
        let rec try_cases : value -> (pattern * expr) list -> value
        = fun v -> function
          | [] -> raise Eval_error 
          | (p, e') :: rest ->
              (match find_match p v with
                | Some nenv -> eval (nenv @ env) e'
                | None -> try_cases v rest) in
                try_cases v cases
  
  | ECons (e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in VCons (v1, v2)
              
  | ETuple es ->
      let vs = List.map (eval env) es in VTuple vs
    
  | ERecFunand (fs, e) ->
    let rec extend_env idx fs env = match fs with
      | [] -> env
      | (f, x, e') :: tail ->
        let env' = (f, Thunk (EValue (VRecFunand (idx, fs, env)), env)) :: env in
        extend_env (idx + 1) tail env'
    in
    let env' = extend_env 1 fs env in
    eval env' e

  

let rec print_value (v: value) : unit = 
  match v with
  | VBool true -> print_string "true"
  | VBool false -> print_string "false"
  | VInt i -> print_int i
  | VFun _ -> print_string "<fun>"
  | VRecFun _ -> print_string "<fun>"
  | VNil -> print_string "[]"
  | VCons (v1, v2) ->
    print_cons v1 v2
  | VTuple vs ->
    print_string "(";
    (match vs with
    | [] -> ()
    | v :: vs ->
        print_value v;
        List.iter (fun v -> print_string ", "; print_value v) vs);
    print_string ")"
  | VRecFunand (_, _, _) -> print_string "<fun>"

    
and print_cons v1 v2 =
  match v2 with
  | VNil ->
    print_value v1;
    print_string " :: []"
  | VCons (_, _) ->
    print_value v1;
    print_string " :: (";
    print_value v2;
    print_string ")"
  | _ ->
    print_value v1;
    print_string " :: ";
    print_value v2

