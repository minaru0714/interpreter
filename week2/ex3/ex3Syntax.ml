type name = string

type binOp = OpAdd | OpSub | OpMul | OpDiv


and value = VInt of int 
            | VBool of bool
            | VFun of name * expr * env
            | VRecFun of name * name * expr * env

          
and env = (name * value) list (*('a * 'b) list*)
           

and expr = EValue of value  
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
          
and command = CExp of expr
          | CLet of name * expr
          | CRecFun of name * name * expr          

and pattern = PInt of int
             | PBool of bool
             | PVar of name


exception Eval_error


let rec find_match : pattern -> value -> env option = fun p v ->
  match p, v with
  | PInt n, VInt m -> if n = m then Some [] else None
  | PBool b1, VBool b2 -> if b1 = b2 then Some [] else None
  | PVar x, _ -> Some [(x, v)]
  | _, _ -> None


let rec eval:env -> expr -> value = fun env expr -> match expr with 
  | EValue e -> e
  | EBin (b, e1, e2) ->
      (match eval env e1 with
        | VBool _ -> raise Eval_error
        | VInt x ->
        (match eval env e2 with
         | VBool _ -> raise Eval_error
         | VInt y ->
        (match b with
         |OpAdd -> VInt (x + y)
         | OpSub -> VInt (x - y)
         | OpMul -> VInt (x * y)
         | OpDiv -> VInt (x / y))))

  | EEqual (e1, e2) ->
      (match eval env e1 with
        | VBool _ -> raise Eval_error
        | VInt x -> (match eval env e2 with
          | VBool _ -> raise Eval_error
          | VInt y -> if x = y then VBool true else VBool false))

  | ECompare (e1, e2) ->
        (match eval env e1 with
            | VBool _ -> raise Eval_error
            | VInt x -> (match eval env e2 with
              | VBool _ -> raise Eval_error
              | VInt y -> if x < y then VBool true else VBool false))

  | EIf (e, e1, e2) ->
    (match eval env e with
        | VInt _ -> raise Eval_error
        | VBool true -> eval env e1
        | VBool false -> eval env e2)
  
  | EVar x -> List.assoc x env  (*List assoc 'a -> ('a * 'b) list -> 'b*)

  | ELet (x, e1, e2) -> eval ((x, eval env e1) :: env) e2  (**)

  | EFun (x, e) -> VFun(x, e, env)

  | EApp (e1,e2) ->
    (match eval env e1 with
    | VBool _ -> raise Eval_error
    | VFun (x, e, oenv) -> 
      eval ((x, eval env e2) :: oenv ) e
    | VRecFun(f, x, e, oenv) ->
       let env' = (x, eval env e2) :: (f, VRecFun (f, x, e, oenv)) :: oenv in eval env' e )
      
  | ERecFun (f, x, e1, e2) ->
    ( let env' = (f, VRecFun(f, x, e1, env)) :: env in eval env' e2)
  

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
  


  let rec print_value : value -> unit  = fun  v ->
        match v with
        | VBool true -> print_string "true"
        | VBool false -> print_string "false"
        | VInt i -> print_int i
        | VFun _ -> print_string "<function>"
        | VRecFun _ -> print_string "<rec_function>"
   