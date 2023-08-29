type value = VInt of int | VBool of bool

type binOp = OpAdd | OpSub | OpMul | OpDiv

type name = string

type expr = EValue of value  
          | EBin of binOp * expr * expr 
          | EEqual of expr * expr
          | ECompare of expr * expr
          | EIf of expr * expr * expr
          | EVar of name
          | ELet of name * expr * expr

type command = CExp of expr
             | CLet of name * expr
             


type env = (name * value) list (*('a * 'b) list*)

exception Eval_error

(*eval : env -> expr -> value*)

let rec eval env = function
    EValue e -> e
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

let rec print_value : value -> unit  = fun  v ->
  match v with
  | VBool true -> print_string "true"
  | VBool false -> print_string "false"
  | VInt i -> print_int i