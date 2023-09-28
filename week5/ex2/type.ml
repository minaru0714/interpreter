open LazySyntax 

type tyvar = string 

type ty = TInt                    
         | TBool                   
         | TFun of ty * ty       
         | TVar of tyvar  
         | TNil
         | TCons of ty * ty
         | TTuple of ty list         

type ty_subst = (tyvar * ty) list

type ty_constraints = (ty * ty) list

type ty_env = (tyvar * ty) list


exception Unification_failure of string

let rec apply_ty_subst (subst: ty_subst) (t: ty): ty =
  let applied = match t with
      | TInt | TBool | TNil -> t
      | TFun (t1, t2) -> TFun (apply_ty_subst subst t1, apply_ty_subst subst t2)
      | TVar v -> (try List.assoc v subst with Not_found -> t)
      | TCons (t1, t2) -> TCons (apply_ty_subst subst t1, apply_ty_subst subst t2)
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
          | (TCons (t1a, t1r), TCons (t2a, t2r)) :: rest -> 
              aux ((t1a, t2a) :: (t1r, t2r) :: rest) subst
          | (TTuple ts1, TTuple ts2) :: rest when List.length ts1 = List.length ts2 -> 
              aux (List.combine ts1 ts2 @ rest) subst    
          | _ :: rest -> raise (Unification_failure "Mismatched types")
        in
        aux constraints []
  

let ty_var_counter = ref 0
        
let new_ty_var () =
   let v = !ty_var_counter in
   ty_var_counter := v + 1;
   "ty" ^ string_of_int v
   
let rec split3 lst =
  match lst with
  | [] -> ([], [], [])
  | (a, b, c) :: rest ->
      let (a', b', c') = split3 rest in
      (a :: a', b :: b', c :: c')


let rec gather_ty_constraints_pattern (p: pattern) : ty * ty_env * ty_constraints =
    match p with
    | PInt _ -> (TInt, [], [])
    | PBool _ -> (TBool, [], [])
    | PVar x -> 
        let new_ty = TVar (new_ty_var ()) in
        (new_ty, [(x, new_ty)], [])
    | PNil -> 
        let new_ty = TNil in
        (new_ty, [], [])
    | PCons (p1, p2) -> 
        let (ty1, env1, constr1) = gather_ty_constraints_pattern p1 in
        let (ty2, env2, constr2) = gather_ty_constraints_pattern p2 in
        let list_type = TCons (ty1, ty2) in
        (list_type, env1 @ env2, constr1 @ constr2)
    | PTuple ps -> 
        let gathered = List.map gather_ty_constraints_pattern ps in
        let tys, envs, constrs = split3 gathered in
        (TTuple tys, List.flatten envs, List.flatten constrs)  



let rec gather_ty_constraints (env: ty_env) (e: expr) : ty * ty_constraints =
  match e with
    | EValue (VInt _) -> (TInt, [])
    | EValue (VBool _) -> (TBool, [])
    | EBin (_, e1, e2) ->
      let (ty1, constraints1) = gather_ty_constraints env e1 in
      let (ty2, constraints2) = gather_ty_constraints env e2 in
      (TInt, (ty1, TInt) :: (ty2, TInt) :: (constraints1 @ constraints2))
    | EVar v -> 
          (try (List.assoc v env, []) 
           with Not_found -> raise (Unification_failure "Variable not found"))
   | EFun (v, e) -> 
        let new_ty = TVar (new_ty_var ()) in
        let (ty, constraints) = gather_ty_constraints ((v, new_ty) :: env) e in
        (TFun (new_ty, ty), constraints)
      | EApp (e1, e2) -> 
        let (ty1, constraints1) = gather_ty_constraints env e1 in
        let (ty2, constraints2) = gather_ty_constraints env e2 in
        let new_ty = TVar (new_ty_var ()) in
        (new_ty, (ty1, TFun (ty2, new_ty)) :: (constraints1 @ constraints2))
      | ELet (v, e1, e2) -> 
        let (ty1, constraints1) = gather_ty_constraints env e1 in
        let env' = (v, ty1) :: env in
        let (ty2, constraints2) = gather_ty_constraints env' e2 in
        (ty2, constraints1 @ constraints2)
    | EIf (e1, e2, e3) -> 
          let (ty1, constraints1) = gather_ty_constraints env e1 in
          let (ty2, constraints2) = gather_ty_constraints env e2 in
          let (ty3, constraints3) = gather_ty_constraints env e3 in
          (ty2, (ty1, TBool) :: (ty2, ty3) :: (constraints1 @ constraints2 @ constraints3))
    | ERecFun (f, v, e1, e2) ->
          let fun_ty = TVar (new_ty_var ()) in
          let (ty1, constraints1) = gather_ty_constraints ((f, fun_ty) :: env) e1 in
          let (ty2, constraints2) = gather_ty_constraints ((f, fun_ty) :: env) e2 in
          (ty2, (fun_ty, TFun(ty1, ty2)) :: (constraints1 @ constraints2))
    | ENil -> 
                let list_ty = TVar (new_ty_var ()) in
                (TCons (list_ty, TNil), [])      
    | ECons (e1, e2) ->
                let (head_ty, constraints1) = gather_ty_constraints env e1 in
                let (tail_ty, constraints2) = gather_ty_constraints env e2 in
                (TCons (head_ty, tail_ty), constraints1 @ constraints2)
    | ETuple es ->
                let tys_and_constraints = List.map (gather_ty_constraints env) es in
                let tys, constraints_lists = List.split tys_and_constraints in
                (TTuple tys, List.flatten constraints_lists)
    | ERecFunand (fns, e) ->
    let fntyvars = List.map (fun (f, v, _) -> (f, TFun (TVar (new_ty_var ()), TVar (new_ty_var ())))) fns in
    let env_with_fns = fntyvars @ env in
    let constraints = List.flatten (List.map2 (fun (f, fun_ty) (_, v, expr) ->
        match fun_ty with
        | TFun (arg_ty, ret_ty) ->
            let (body_ty, constraints) = gather_ty_constraints ((f, fun_ty) :: (v, arg_ty) :: env_with_fns) expr in
            (ret_ty, body_ty) :: constraints
        | _ -> failwith "Unexpected type"
    ) fntyvars fns) in
    let (rest_ty, rest_constraints) = gather_ty_constraints env_with_fns e in
    (rest_ty, constraints @ rest_constraints)
      | EMatch (e, branches) ->
          let (matched_ty, matched_constraints) = gather_ty_constraints env e in
          let branch_tys_and_constraints = List.map (fun (p, e_branch) -> 
              let (pat_ty, pat_env, pat_constraints) = gather_ty_constraints_pattern p in
              let (branch_ty, branch_constraints) = gather_ty_constraints (pat_env @ env) e_branch in
              (branch_ty, (matched_ty, pat_ty) :: (branch_constraints @ pat_constraints))
          ) branches in
          let branch_tys, branch_constraints_list = List.split branch_tys_and_constraints in
          let all_constraints = matched_constraints @ (List.flatten branch_constraints_list) in
          let expected_ty = TVar (new_ty_var ()) in
          (expected_ty, List.map (fun ty -> (ty, expected_ty)) branch_tys @ all_constraints)
      | _ -> failwith "Not yet implemented for this expression"
  



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
          ((v, ty) :: env, new_env)
      | CRecFun (f, v, e) -> 
          let (ty, new_env) = infer_expr env e in
          ((f, ty) :: env, new_env)



let rec print_type (t: ty): unit =
    match t with
    | TInt -> print_string "Int"
    | TBool -> print_string "Bool"
    | TFun (t1, t2) ->
        print_type t1;
        print_string " -> ";
        print_type t2
    | TVar v -> print_string v
    | TNil -> print_string "list"
    | TCons (t1, t2) ->
        print_string " [";
        print_type t1;
        print_string " ]"
    | TTuple ts -> 
        print_string "(";
        List.iteri (fun idx ty ->
            if idx > 0 then print_string " * ";
            print_type ty
        ) ts;
        print_string ")"
      