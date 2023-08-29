type value = VInt of int
           | VBool of bool

type binOp = OpAdd | OpSub | OpMul | OpDiv

type expr = EValue of value  
          | EBin of binOp * expr * expr 
          | EEqual of expr * expr
          | ECompare of expr * expr
          | EIf of expr * expr * expr

exception Eval_error (*例外処理*)

let rec eval : expr->value = function
    EValue e -> e
  | EBin (b, e1, e2) ->
      (match eval e1 with
        | VBool _ -> raise Eval_error
        | VInt x ->
            (match eval e2 with
                | VBool _ -> raise Eval_error
                | VInt y ->
                    (match b with
                    OpAdd -> VInt (x + y)
                  | OpSub -> VInt (x - y)
                  | OpMul -> VInt (x * y)
                  | OpDiv -> VInt (x / y))))
  | EEqual (e1, e2) ->
      (match eval e1 with
        | VBool _ -> raise Eval_error
        | VInt x -> (match eval e2 with
                        | VBool _ -> raise Eval_error
                        | VInt y -> if x = y then VBool true else VBool false))
  | ECompare (e1, e2) ->
        (match eval e1 with
            | VBool _ -> raise Eval_error
            | VInt x -> (match eval e2 with
                            | VBool _ -> raise Eval_error
                            | VInt y -> if x < y then VBool true else VBool false))
  | EIf (e, e1, e2) ->
    (match eval e with
        | VInt _ -> raise Eval_error
        | VBool true -> eval e1
        | VBool false -> eval e2)

let rec print_expr ( e : expr ) : unit = match eval e with
  | VBool true -> print_string "true"
  | VBool false -> print_string "false"
  | VInt i -> print_int i
