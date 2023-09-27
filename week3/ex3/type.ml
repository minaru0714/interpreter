
type tyvar = string 

type ty = TInt                    
         | TBool                   
         | TFun of ty * ty       
         | TVar of tyvar           

type ty_subst = (tyvar * ty) list

type ty_constraints = (ty * ty) list

exception Unification_failure of string

let rec apply_ty_subst (subst: ty_subst) (t: ty): ty =
    let applied = match t with
        | TInt | TBool -> t
        | TFun (t1, t2) -> TFun (apply_ty_subst subst t1, apply_ty_subst subst t2)
        | TVar v -> (try List.assoc v subst with Not_found -> t)
    in
    if applied = t then t else apply_ty_subst subst applied

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
          | _ :: rest -> raise (Unification_failure "Mismatched types")
        in
        aux constraints []
  

 let rec print_type ty =
   match ty with
   | TInt -> print_string "int"
   | TBool -> print_string "bool"
   | TFun (t1, t2) ->
      print_type t1;
      print_string " -> ";
      print_type t2
   | TVar v -> print_string v