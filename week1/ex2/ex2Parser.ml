type token =
  | INT of (int)
  | BOOL of (bool)
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | EQ
  | LT
  | IF
  | THEN
  | ELSE
  | LPAR
  | RPAR
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "ex2Parser.mly"
    open Ex2Syntax
# 22 "ex2Parser.ml"
let yytransl_const = [|
  259 (* PLUS *);
  260 (* MINUS *);
  261 (* TIMES *);
  262 (* DIV *);
  263 (* EQ *);
  264 (* LT *);
  265 (* IF *);
  266 (* THEN *);
  267 (* ELSE *);
  268 (* LPAR *);
  269 (* RPAR *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* BOOL *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\004\000\004\000\
\004\000\005\000\005\000\005\000\006\000\006\000\006\000\000\000"

let yylen = "\002\000\
\002\000\006\000\001\000\003\000\003\000\001\000\003\000\003\000\
\001\000\003\000\003\000\001\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\013\000\014\000\000\000\000\000\016\000\000\000\
\003\000\000\000\000\000\012\000\000\000\000\000\001\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\015\000\000\000\
\000\000\000\000\000\000\010\000\011\000\000\000\000\000\002\000"

let yydgoto = "\002\000\
\007\000\008\000\009\000\010\000\011\000\012\000"

let yysindex = "\255\255\
\004\255\000\000\000\000\000\000\004\255\004\255\000\000\004\000\
\000\000\007\255\015\255\000\000\255\254\009\255\000\000\006\255\
\006\255\006\255\006\255\006\255\006\255\004\255\000\000\015\255\
\015\255\021\255\021\255\000\000\000\000\023\255\004\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\027\000\001\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\012\000\
\023\000\031\000\035\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\253\255\000\000\011\000\016\000\017\000"

let yytablesize = 304
let yytable = "\001\000\
\009\000\013\000\014\000\015\000\003\000\004\000\003\000\004\000\
\022\000\016\000\017\000\007\000\005\000\018\000\019\000\006\000\
\000\000\006\000\030\000\020\000\021\000\023\000\008\000\016\000\
\017\000\000\000\006\000\032\000\026\000\027\000\004\000\024\000\
\025\000\031\000\005\000\000\000\028\000\029\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\009\000\009\000\000\000\000\000\009\000\
\009\000\000\000\009\000\009\000\000\000\009\000\007\000\007\000\
\000\000\000\000\007\000\007\000\000\000\007\000\007\000\000\000\
\007\000\008\000\008\000\000\000\000\000\008\000\008\000\000\000\
\008\000\008\000\000\000\008\000\006\000\006\000\000\000\006\000\
\004\000\004\000\000\000\004\000\005\000\005\000\000\000\005\000"

let yycheck = "\001\000\
\000\000\005\000\006\000\000\000\001\001\002\001\001\001\002\001\
\010\001\003\001\004\001\000\000\009\001\007\001\008\001\012\001\
\255\255\012\001\022\000\005\001\006\001\013\001\000\000\003\001\
\004\001\255\255\000\000\031\000\018\000\019\000\000\000\016\000\
\017\000\011\001\000\000\255\255\020\000\021\000\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\003\001\004\001\255\255\255\255\007\001\
\008\001\255\255\010\001\011\001\255\255\013\001\003\001\004\001\
\255\255\255\255\007\001\008\001\255\255\010\001\011\001\255\255\
\013\001\003\001\004\001\255\255\255\255\007\001\008\001\255\255\
\010\001\011\001\255\255\013\001\010\001\011\001\255\255\013\001\
\010\001\011\001\255\255\013\001\010\001\011\001\255\255\013\001"

let yynames_const = "\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  EQ\000\
  LT\000\
  IF\000\
  THEN\000\
  ELSE\000\
  LPAR\000\
  RPAR\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  BOOL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 18 "ex2Parser.mly"
         (_1)
# 183 "ex2Parser.ml"
               : Ex2Syntax.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 22 "ex2Parser.mly"
                                 ( EIf(_2,_4,_6) )
# 192 "ex2Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'compare_expr) in
    Obj.repr(
# 23 "ex2Parser.mly"
                                 ( _1 )
# 199 "ex2Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 27 "ex2Parser.mly"
                             ( EEqual(_1, _3) )
# 207 "ex2Parser.ml"
               : 'compare_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 28 "ex2Parser.mly"
                             ( ECompare(_1, _3) )
# 215 "ex2Parser.ml"
               : 'compare_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 29 "ex2Parser.mly"
                             ( _1 )
# 222 "ex2Parser.ml"
               : 'compare_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term_expr) in
    Obj.repr(
# 33 "ex2Parser.mly"
                               ( EBin(OpAdd, _1, _3) )
# 230 "ex2Parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term_expr) in
    Obj.repr(
# 34 "ex2Parser.mly"
                               ( EBin(OpSub, _1, _3) )
# 238 "ex2Parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term_expr) in
    Obj.repr(
# 35 "ex2Parser.mly"
                               ( _1 )
# 245 "ex2Parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expr) in
    Obj.repr(
# 39 "ex2Parser.mly"
                                ( EBin(OpMul, _1, _3) )
# 253 "ex2Parser.ml"
               : 'term_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expr) in
    Obj.repr(
# 40 "ex2Parser.mly"
                                ( EBin(OpDiv, _1, _3) )
# 261 "ex2Parser.ml"
               : 'term_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expr) in
    Obj.repr(
# 41 "ex2Parser.mly"
                               ( _1 )
# 268 "ex2Parser.ml"
               : 'term_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 45 "ex2Parser.mly"
                    ( EValue (VInt _1) )
# 275 "ex2Parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 46 "ex2Parser.mly"
                    ( EValue (VBool _1) )
# 282 "ex2Parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 47 "ex2Parser.mly"
                    ( _2 )
# 289 "ex2Parser.ml"
               : 'atomic_expr))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ex2Syntax.expr)
