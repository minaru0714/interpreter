open  Ex5Syntax

type tyvar = string 

type ty = TInt                    
         | TBool                   
         | TFun of ty * ty       
         | TVar of tyvar  
         | TList of ty 
         | TTuple of ty list         

type ty_subst = (tyvar * ty) list

type ty_constraints = (ty * ty) list

type ty_env = (tyvar * ty) list


exception Unification_failure of string

let rec apply_ty_subst (subst: ty_subst) (t: ty): ty =
  let applied = match t with
      | TInt | TBool  -> t
      | TFun (t1, t2) -> TFun (apply_ty_subst subst t1, apply_ty_subst subst t2)
      | TVar v -> (try List.assoc v subst with Not_found -> t)
      | TList t -> TList (apply_ty_subst subst t)
      | TTuple ts -> TTuple (List.map (apply_ty_subst subst) ts)
  in
  applied    

let compose_ty_subst (s1: ty_subst) (s2: ty_subst): ty_subst =
       let applied_s2 = List.map (fun (var, t) -> (var, apply_ty_subst s1 t)) s2 in
       let filtered_s1 = List.filter (fun (v, _) -> not (List.mem_assoc v applied_s2)) s1 in
        filtered_s1 @ applied_s2

let occurs_in v t subst = 
   let rec aux t = 
      match t with
      | TVar v' when v = v' -> true
      | TVar v' -> (try aux (List.assoc v' subst) with Not_found -> false)
      | TFun (t1, t2) -> (aux t1) || (aux t2)
      | TList t' -> aux t'
      | TTuple ts -> List.exists aux ts
      | _ -> false
     in aux t

let rec ty_unify (constraints: ty_constraints) : ty_subst =
    let rec aux constraints subst =
          match constraints with
          | [] -> subst
          | (t1, t2) :: rest when t1 = t2 -> aux rest subst
          | (TVar v, t) :: rest when not (occurs_in v t subst) ->
              let new_subst = [(v, t)] in
              aux (List.map (fun (t1', t2') -> (apply_ty_subst new_subst t1', apply_ty_subst new_subst t2')) rest) (compose_ty_subst new_subst subst)
          | (t, TVar v) :: rest when not (occurs_in v t subst) ->
              let new_subst = [(v, t)] in
              aux (List.map (fun (t1', t2') -> (apply_ty_subst new_subst t1', apply_ty_subst new_subst t2')) rest) (compose_ty_subst new_subst subst)
          | (TFun (t1a, t1r), TFun (t2a, t2r)) :: rest -> 
              aux ((t1a, t2a) :: (t1r, t2r) :: rest) subst
          | (TList (t1a),  TList (t2a)) :: rest -> 
              aux ((t1a, t2a)  :: rest) subst 
          | (TTuple ts1, TTuple ts2) :: rest when List.length ts1 = List.length ts2 -> 
              aux (List.combine ts1 ts2 @ rest) subst    
          | _ :: rest -> raise (Unification_failure "Mismatched types")
        in
        aux constraints []
  

let ty_var_counter = ref 1
        
let new_ty_var () =
   let v = !ty_var_counter in
   ty_var_counter := v + 1;
   "t" ^ string_of_int v

   let rec gather_ty_constraints_pattern (p: pattern) : ty * ty_env * ty_constraints =
    match p with
    | PInt _ -> (TInt, [], [])
    | PBool _ -> (TBool, [], [])
    | PVar x -> 
        let t = TVar (new_ty_var ()) in
        (t, [(x, t)], [])
    | PNil -> (TList (TVar (new_ty_var ())), [], [])
    | PCons (p1, p2) -> 
        let (head_ty, head_env, head_constraints) = gather_ty_constraints_pattern p1 in
        let (tail_ty, tail_env, tail_constraints) = gather_ty_constraints_pattern p2 in
        (TList head_ty, head_env @ tail_env, (TList head_ty, tail_ty) :: head_constraints @ tail_constraints)
    | PTuple ps ->
        let pts = List.map gather_ty_constraints_pattern ps in
        let ts = List.map (fun (t, _, _) -> t) pts in
        let envs = List.map (fun (_, env, _) -> env) pts in
        let constrs = List.map (fun (_, _, c) -> c) pts in
        (TTuple ts, List.flatten envs, List.flatten constrs)



let rec gather_ty_constraints ty_env = function
    | EValue v ->
        (match v with 
        | VInt _ -> (TInt, [])
        | VBool _ -> (TBool, [])
        | _ -> raise ((Unification_failure "not value"))
        )
    | EBin (op, e1, e2) ->
        let t1, c1 = gather_ty_constraints ty_env e1 in
        let t2, c2 = gather_ty_constraints ty_env e2 in
        (match op with
        | OpAdd | OpSub | OpMul | OpDiv  -> (TInt, (t1, TInt) :: (t2, TInt) :: c1 @ c2)
        )
    | EEqual (e1, e2) | ECompare (e1, e2) ->
        let t1, c1 = gather_ty_constraints ty_env e1 in
        let t2, c2 = gather_ty_constraints ty_env e2 in
        (TBool, (t1, t2) :: c1 @ c2)
    | EIf (e1, e2, e3) ->
        let t1, c1 = gather_ty_constraints ty_env e1 in
        let t2, c2 = gather_ty_constraints ty_env e2 in
        let t3, c3 = gather_ty_constraints ty_env e3 in
        (t2, (t1, TBool) :: (t2, t3) :: c1 @ c2 @ c3)
    | EVar x -> 
        (List.assoc x ty_env, [])
    | ELet (x, e1, e2) ->
        let t1, c1 = gather_ty_constraints ty_env e1 in
        let t2, c2 = gather_ty_constraints ((x, t1) :: ty_env) e2 in
        (t2, c1 @ c2)
    | EFun (x, e) ->
        let t_var_arg = TVar (new_ty_var ()) in
        let t_var_res = TVar (new_ty_var ()) in
        let t, c = gather_ty_constraints ((x, t_var_arg) :: ty_env) e in
        (TFun (t_var_arg, t_var_res), (t_var_res, t) :: c)
    | EApp (e1, e2) ->
        let t_var = TVar (new_ty_var ()) in
        let t1, c1 = gather_ty_constraints ty_env e1 in
        let t2, c2 = gather_ty_constraints ty_env e2 in
        (t_var, (t1, TFun(t2, t_var)) :: c1 @ c2)
    | ECons (e1, e2) ->
        let (head_ty, constraints1) = gather_ty_constraints ty_env e1 in
        let (tail_ty, constraints2) = gather_ty_constraints ty_env e2 in
        (TList (head_ty), constraints1 @ constraints2 @ [TList(head_ty), tail_ty])
    | ETuple es -> 
        let tcs = List.map (gather_ty_constraints ty_env) es in
        let ts = List.map fst tcs in
        let cs = List.flatten (List.map snd tcs) in
        (TTuple ts, cs)
    | ERecFun (f, x, e1, e2) ->
        let t_var_arg = TVar (new_ty_var ()) in
        let t_var_res = TVar (new_ty_var ()) in
        let t1, c1 = gather_ty_constraints ((f, TFun(t_var_arg, t_var_res)) :: (x, t_var_arg) :: ty_env) e1 in
        let t2, c2 = gather_ty_constraints ((f, TFun(t_var_arg, t1)) :: ty_env) e2 in
        (t2, (TFun(t_var_arg, t_var_res), TFun(t_var_arg, t1)) :: c1 @ c2)
    | ERecFunand (bindings, e) ->
        let new_ty_env = List.fold_left (fun acc (f, x, _) -> 
            let t_var_name_f = new_ty_var () in
            let t_var_name_x = new_ty_var () in
            let t_var_f = TVar t_var_name_f in  
            let t_var_x = TVar t_var_name_x in  
            (f, TFun(t_var_x, t_var_f)) :: (x, t_var_x) :: acc
        ) ty_env bindings in
        let cs = List.flatten (List.map (fun (f, x, ex) -> 
            let t, c = gather_ty_constraints new_ty_env ex in
            c @ [(List.assoc f new_ty_env, TFun(List.assoc x new_ty_env, t))]
        ) bindings) in
        let t, c = gather_ty_constraints new_ty_env e in
        (t, cs @ c)
    
      | EMatch (e, branches) ->
          let (matched_ty, matched_constraints) = gather_ty_constraints ty_env e in
          let branch_tys_and_constraints = List.map (fun (p, e_branch) -> 
              let (pat_ty, pat_env, pat_constraints) = gather_ty_constraints_pattern p in
              let (branch_ty, branch_constraints) = gather_ty_constraints (pat_env @ ty_env) e_branch in
              (branch_ty, (matched_ty, pat_ty) :: (branch_constraints @ pat_constraints))
          ) branches in
          let branch_tys, branch_constraints_list = List.split branch_tys_and_constraints in
          let all_constraints = matched_constraints @ (List.flatten branch_constraints_list) in
          let expected_ty = TVar (new_ty_var ()) in
          (expected_ty, List.map (fun ty -> (ty, expected_ty)) branch_tys @ all_constraints)
    
  

   
let infer_expr (env: ty_env) (e: expr) : ty * ty_env =
      try 
          let (ty, constraints) = gather_ty_constraints env e in
          let subst = ty_unify constraints in
          (apply_ty_subst subst ty, List.map (fun (v, t) -> (v, apply_ty_subst subst t)) env)
      with 
      | Unification_failure msg -> failwith ("Type inference failed: " ^ msg)


let infer_cmd (env: ty_env) (cmd: command) : ty_env * ty_env =
      match cmd with
      | CExp e -> 
          let (ty, new_env) = infer_expr env e in
          (env, new_env)   
      | CLet (v, e) -> 
          let (ty, new_env) = infer_expr env e in
          ([(v, ty)], new_env)
      | CRecFun(f, v, e) ->   
          let t_var_arg = TVar (new_ty_var ()) in
          let t_var_res = TVar (new_ty_var ()) in
          let (ty, new_env) = infer_expr ((f, TFun(t_var_arg, t_var_res)) :: (v, t_var_arg) :: env) e in
          let print_env = [(f, ty)] in
          (print_env , new_env)



let rec print_type (t: ty): unit =
    match t with
    | TInt -> print_string "Int"
    | TBool -> print_string "Bool"
    | TFun (t1, t2) ->
        print_type t1;
        print_string " -> ";
        print_type t2
    | TVar v -> print_string v
    | TList (t1) ->
        print_type t1;
        print_string " list"
    | TTuple ts -> 
        print_string "(";
        List.iteri (fun idx ty ->
            if idx > 0 then print_string " * ";
            print_type ty
        ) ts;
        print_string ")"
      