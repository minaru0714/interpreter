
type name = string

type binOp = OpAdd | OpSub | OpMul | OpDiv 

type pattern = PInt of int
             | PBool of bool
             | PUnit
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
          | EUnit
          | ELet of name * expr * expr
          | EFun of name * expr
          | EApp of expr * expr
          | ERecFun of  name * name * expr * expr
          | EMatch of expr * (pattern * expr) list
          | ECons of expr * expr          (* e1 :: e2 *)
          | ETuple of expr list           (* (e1, e2, ..., en) *)
          | ERecFunand of (name * name * expr) list * expr
          | ELetRec of (name * expr) list * expr

and env = (name * thunk) list
and value = VInt of int
          | VBool of bool
          | VUnit
          | VFun of name * expr * env
          | VRecFun of name * name * expr * env
          | VNil
          | VCons of value * value
          | VTuple of value list
          | VRecFunand of int * (name * name * expr) list * env


and thunk = Thunk of expr * env
and lazy_value =  LVInt of int
                | LVBool of bool
                | LVUnit
                | LVFun of name * expr * env
                | LVRecFun of name * name * expr * env
                | LVNil
                | LVCons of thunk * thunk
                | LVTuple of thunk list
                | LVRecFunand of int * (name * name * expr) list * env


type command = CExp of expr
             | CLet of name * expr
             | CRecFun of name * name * expr
             | CRecFunand  of (name * name * expr) list
             | CLetRec of (name * expr) list



exception Eval_error

let rec find_match : pattern -> thunk -> env option = fun p t ->
  match p with
  | PVar x -> Some [(x, t)]
  | PUnit -> (match force t with LVUnit -> Some [] | _ -> None) 
  | _ -> let v = match t with
   |Thunk(e, env) -> eval env e in
   match p,v with 
    | PInt n, LVInt m -> if n = m then Some [] else None
    | PBool b1, LVBool b2 -> if b1 = b2 then Some [] else None
    | PNil, LVNil -> Some []
    | PCons(p1, p2), LVCons(v1, v2) ->
      (match find_match p1 v1 , find_match p2 v2 with
      | Some env1, Some env2 -> Some (env1 @ env2)
      | _, _ -> None)
    | PTuple ps, LVTuple vs ->
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


and force_to_value : lazy_value -> value = function
  | LVInt i -> VInt i
  | LVBool b -> VBool b
  | LVUnit -> VUnit
  | LVFun (x, e, env) -> VFun (x, e, env)
  | LVRecFun (f, x, e, env) -> VRecFun (f, x, e, env)
  | LVNil -> VNil
  | LVCons (th1, th2) -> VCons (force_to_value (force th1), force_to_value (force th2))
  | LVTuple ths -> VTuple (List.map (fun th -> force_to_value (force th)) ths)
  | LVRecFunand (idx, fns, env) -> VRecFunand (idx, fns, env)

and force thunk = match thunk with
  | Thunk (e, env) -> eval env e

and eval : env -> expr -> lazy_value = fun env expr ->
  match expr with
  | EValue v -> (match v with
                 | VInt i -> LVInt i
                 | VBool b -> LVBool b
                 | VUnit -> LVUnit
                 | VFun (x, e, env) -> LVFun (x, e, env)
                 | VRecFun (f, x, e, env) -> LVRecFun (f, x, e, env)
                 | VNil -> LVNil
                 | VCons (v1, v2) -> LVCons (Thunk (EValue v1, env), Thunk (EValue v2, env))
                 | VTuple vs -> LVTuple (List.map (fun v -> Thunk (EValue v, env)) vs)
                 | VRecFunand (idx, fns, env) -> LVRecFunand (idx, fns, env)
                )
  | EBin (b, e1, e2) ->
  (match eval env e1 with
    | LVBool _ -> raise Eval_error
    | LVInt x ->
      (match eval env e2 with
       | LVBool _ -> raise Eval_error
       | LVInt y ->
         (match b with
          | OpAdd -> LVInt (x + y)
          | OpSub -> LVInt (x - y)
          | OpMul -> LVInt (x * y)
          | OpDiv -> LVInt (x / y)
         )
       | _ -> raise Eval_error
      )
    | _ -> raise Eval_error  
  )
  
| EEqual (e1, e2) ->
  (match eval env e1 with
    | LVBool _ -> raise Eval_error
    | LVInt x -> 
      (match eval env e2 with
       | LVBool _ -> raise Eval_error
       | LVInt y -> if x = y then LVBool true else LVBool false
       | _ -> raise Eval_error  
      )
    | _ -> raise Eval_error  )
  
| ECompare (e1, e2) ->
  (match eval env e1 with
    | LVBool _ -> raise Eval_error
    | LVInt x -> 
      (match eval env e2 with
       | LVBool _ -> raise Eval_error
       | LVInt y -> if x < y then LVBool true else LVBool false
       | _ -> raise Eval_error  
      )
    | _ -> raise Eval_error  
  )
  
| EIf (e, e1, e2) ->
  (match eval env e with
    | LVInt _ -> raise Eval_error
    | LVBool true -> eval env e1
    | LVBool false -> eval env e2
    | _ -> raise Eval_error  
  )
| EVar x -> 
    let thk = List.assoc x env in
    force thk

| EUnit -> LVUnit

| ELet (x, e1, e2) ->
    let lv1 = eval env e1 in
    let v1 = force_to_value lv1 in
    eval ((x, Thunk (EValue v1, [])) :: env) e2

| EFun (x, e) ->
    LVFun (x, e, env)
    
| EApp (e1, e2) ->
    (match eval env e1 with
    | LVFun (x, e, oenv) -> 
        let thk = Thunk (e2, env) in  
        eval ((x, thk) :: oenv) e
    | LVRecFun(f, x, e, oenv) ->
        let env' = (x, Thunk (e2, env)) :: (f, Thunk (EValue (VRecFun (f, x, e, oenv)), oenv)) :: oenv in 
        eval env' e
    | LVRecFunand (idx, fns, oenv) ->
        let (f, x, e) = List.nth fns (idx - 1) in
        let env' = (x, Thunk (e2, env)) :: (List.mapi (fun i (f, x, e) -> 
            (f, Thunk (EValue (VRecFunand (i+1, fns, oenv)), oenv))) fns) @ oenv in
        eval env' e
    | _ -> raise Eval_error)

 | ERecFun (f, x, e1, e2) ->
    let env' = (f, Thunk (EValue (VRecFun(f, x, e1, env)), env)) :: env in
    eval env' e2

 | EMatch (e, cases) -> 
    (let t = Thunk(e, env) in
    let rec try_cases : (pattern * expr) list -> lazy_value
    =  function
      | [] -> raise Eval_error 
      | (p, e') :: rest ->
          (match find_match p t with
            | Some nenv -> eval (nenv @ env) e'
            | None -> try_cases rest) in
    try_cases  cases)

  
  | ECons (e1, e2) -> LVCons (Thunk (e1, env), Thunk (e2, env))

  | ETuple es -> LVTuple (List.map (fun e -> Thunk (e, env)) es)

  | ERecFunand (functions, e) ->
   (let rec extend_env idx fs env = 
   match fs with
      | [] -> env
      | (f, x, e') :: tail ->
        let env' = (f, Thunk (EValue (VRecFunand (idx, functions, env)), env)) :: env in
        extend_env (idx + 1) tail env'
    in
    let env' = extend_env 1 functions env in
    eval env' e)
  
  | ELetRec (bindings, e)  ->
  let rec extend_env env = function
    | [] -> env
    | (f, e1) :: rest ->
      let env' = (f, Thunk(e1, env)) :: env in
      extend_env env' rest
  in
  let env' = extend_env env bindings in
  eval env' e

  (*
  | ELetRec ([(f, e1)], e2)  ->
    let rec env' = (f, Thunk(e1, env')) :: env in
    eval env' e2
    *)

let rec print_value (v: value) : unit = 
  match v with
  | VBool true -> print_string "true"
  | VBool false -> print_string "false"
  | VInt i -> print_int i
  | VUnit -> print_string "()"
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

