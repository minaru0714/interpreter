type token =
  | INT of (int)
  | BOOL of (bool)
  | ID of (string)
  | LET
  | IN
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
  | FUN
  | ARROW
  | MATCH
  | WITH
  | OR
  | END
  | REC
  | SEMI
  | EOF
  | COMMA
  | CONS
  | LBRACKET
  | RBRACKET
  | AND

open Parsing;;
let _ = parse_error;;
# 2 "ex4Parser.mly"
  open Ex4Syntax
# 38 "ex4Parser.ml"
let yytransl_const = [|
  260 (* LET *);
  261 (* IN *);
  262 (* PLUS *);
  263 (* MINUS *);
  264 (* TIMES *);
  265 (* DIV *);
  266 (* EQ *);
  267 (* LT *);
  268 (* IF *);
  269 (* THEN *);
  270 (* ELSE *);
  271 (* LPAR *);
  272 (* RPAR *);
  273 (* FUN *);
  274 (* ARROW *);
  275 (* MATCH *);
  276 (* WITH *);
  277 (* OR *);
  278 (* END *);
  279 (* REC *);
  280 (* SEMI *);
    0 (* EOF *);
  281 (* COMMA *);
  282 (* CONS *);
  283 (* LBRACKET *);
  284 (* RBRACKET *);
  285 (* AND *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* BOOL *);
  259 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\005\000\005\000\
\006\000\006\000\009\000\009\000\010\000\010\000\011\000\011\000\
\008\000\008\000\008\000\012\000\012\000\012\000\012\000\012\000\
\012\000\013\000\013\000\007\000\007\000\007\000\007\000\007\000\
\004\000\000\000\000\000"

let yylen = "\002\000\
\002\000\002\000\004\000\006\000\007\000\005\000\007\000\009\000\
\006\000\003\000\003\000\003\000\003\000\003\000\003\000\004\000\
\005\000\002\000\005\000\003\000\001\000\001\000\002\000\002\000\
\005\000\000\000\001\000\003\000\001\000\005\000\001\000\003\000\
\000\000\003\000\005\000\001\000\001\000\001\000\002\000\005\000\
\003\000\001\000\003\000\001\000\001\000\001\000\003\000\002\000\
\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\044\000\045\000\046\000\000\000\000\000\
\000\000\000\000\000\000\000\000\050\000\000\000\022\000\021\000\
\027\000\000\000\051\000\000\000\049\000\000\000\000\000\000\000\
\000\000\000\000\000\000\048\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\001\000\000\000\018\000\000\000\000\000\
\002\000\000\000\000\000\000\000\000\000\000\000\047\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\021\000\000\000\000\000\000\000\000\000\
\024\000\000\000\000\000\000\000\000\000\000\000\000\000\036\000\
\037\000\038\000\000\000\000\000\000\000\000\000\000\000\003\000\
\000\000\000\000\000\000\000\000\000\000\030\000\000\000\000\000\
\039\000\019\000\000\000\000\000\000\000\000\000\000\000\000\000\
\032\000\000\000\000\000\000\000\004\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\005\000\000\000\000\000\
\040\000\035\000\000\000\000\000\043\000\000\000\025\000"

let yydgoto = "\003\000\
\013\000\019\000\068\000\044\000\045\000\103\000\015\000\077\000\
\016\000\017\000\069\000\078\000\107\000"

let yysindex = "\021\000\
\196\255\168\001\000\000\000\000\000\000\000\000\039\255\196\255\
\196\255\253\254\196\255\245\254\000\000\249\000\000\000\000\000\
\000\000\064\255\000\000\219\255\000\000\253\254\083\255\015\001\
\177\255\011\255\037\001\000\000\196\255\196\255\196\255\196\255\
\196\255\196\255\196\255\000\000\196\255\000\000\253\254\083\255\
\000\000\253\254\196\255\083\255\022\255\196\255\000\000\196\255\
\196\255\062\255\017\255\001\255\001\255\089\255\089\255\057\255\
\057\255\059\001\057\255\000\000\253\254\016\255\120\255\152\001\
\000\000\196\255\081\001\103\001\073\255\152\001\196\255\000\000\
\000\000\000\000\017\255\066\255\075\255\043\255\120\255\000\000\
\196\255\095\255\152\001\196\255\196\255\000\000\152\001\029\255\
\000\000\000\000\196\255\017\255\019\255\152\001\196\255\152\001\
\000\000\017\255\130\001\076\255\000\000\253\254\079\255\100\255\
\152\001\031\255\098\255\017\255\253\254\000\000\196\255\017\255\
\000\000\000\000\083\255\152\001\000\000\097\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\107\255\051\000\076\000\001\000\026\000\098\000\
\119\000\000\000\140\000\000\000\000\000\000\000\000\000\072\255\
\000\000\000\000\000\000\115\255\000\000\154\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\168\000\000\000\000\000\000\000\182\000\000\000\
\000\000\000\000\000\000\000\000\000\000\072\255\000\000\196\000\
\000\000\000\000\111\255\109\255\000\000\000\000\000\000\000\000\
\210\000\119\255\000\000\107\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\224\000\000\000\047\255\000\000"

let yygindex = "\000\000\
\000\000\000\000\004\000\110\000\223\255\211\255\054\000\030\000\
\102\000\000\000\057\000\189\255\031\000"

let yytablesize = 707
let yytable = "\021\000\
\014\000\004\000\005\000\006\000\014\000\020\000\062\000\088\000\
\031\000\032\000\065\000\024\000\025\000\021\000\027\000\035\000\
\028\000\072\000\073\000\074\000\066\000\001\000\002\000\095\000\
\100\000\015\000\066\000\012\000\049\000\082\000\106\000\075\000\
\052\000\053\000\054\000\055\000\056\000\057\000\058\000\080\000\
\059\000\021\000\101\000\076\000\106\000\093\000\064\000\102\000\
\104\000\067\000\012\000\026\000\070\000\098\000\092\000\112\000\
\092\000\004\000\005\000\006\000\091\000\022\000\029\000\030\000\
\031\000\032\000\021\000\038\000\092\000\083\000\026\000\035\000\
\119\000\038\000\087\000\013\000\023\000\038\000\038\000\071\000\
\038\000\118\000\037\000\012\000\094\000\021\000\039\000\096\000\
\086\000\004\000\005\000\006\000\043\000\089\000\099\000\023\000\
\090\000\010\000\105\000\095\000\023\000\092\000\110\000\035\000\
\111\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
\038\000\113\000\116\000\012\000\023\000\038\000\011\000\026\000\
\038\000\038\000\021\000\038\000\041\000\102\000\041\000\040\000\
\033\000\081\000\031\000\042\000\034\000\041\000\042\000\050\000\
\038\000\114\000\060\000\020\000\038\000\097\000\117\000\000\000\
\000\000\000\000\000\000\038\000\061\000\038\000\000\000\063\000\
\038\000\016\000\000\000\000\000\000\000\000\000\038\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\006\000\
\000\000\038\000\079\000\000\000\000\000\000\000\000\000\000\000\
\000\000\004\000\005\000\006\000\000\000\017\000\029\000\030\000\
\031\000\032\000\033\000\034\000\000\000\000\000\000\000\035\000\
\047\000\000\000\000\000\009\000\004\000\005\000\006\000\007\000\
\000\000\048\000\037\000\012\000\000\000\000\000\000\000\008\000\
\000\000\007\000\009\000\109\000\010\000\000\000\011\000\000\000\
\000\000\000\000\115\000\004\000\005\000\006\000\012\000\008\000\
\029\000\030\000\031\000\032\000\033\000\034\000\000\000\000\000\
\000\000\035\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\041\000\000\000\037\000\012\000\000\000\000\000\
\036\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\014\000\014\000\014\000\
\014\000\014\000\014\000\014\000\000\000\014\000\014\000\000\000\
\014\000\000\000\000\000\000\000\014\000\014\000\014\000\000\000\
\014\000\014\000\014\000\000\000\000\000\014\000\015\000\015\000\
\015\000\015\000\015\000\015\000\015\000\000\000\015\000\015\000\
\000\000\015\000\000\000\000\000\000\000\015\000\015\000\015\000\
\000\000\015\000\015\000\015\000\000\000\000\000\015\000\012\000\
\012\000\012\000\000\000\000\000\012\000\012\000\000\000\012\000\
\012\000\000\000\012\000\000\000\000\000\000\000\012\000\012\000\
\012\000\000\000\012\000\012\000\012\000\000\000\000\000\012\000\
\013\000\013\000\013\000\000\000\000\000\013\000\013\000\000\000\
\013\000\013\000\000\000\013\000\000\000\000\000\000\000\013\000\
\013\000\013\000\000\000\013\000\013\000\013\000\010\000\000\000\
\013\000\000\000\000\000\010\000\010\000\000\000\010\000\010\000\
\000\000\010\000\000\000\000\000\000\000\010\000\010\000\010\000\
\000\000\010\000\010\000\011\000\000\000\000\000\010\000\000\000\
\011\000\011\000\000\000\011\000\011\000\000\000\011\000\000\000\
\000\000\000\000\011\000\011\000\011\000\000\000\011\000\011\000\
\020\000\000\000\000\000\011\000\000\000\020\000\020\000\000\000\
\020\000\020\000\000\000\020\000\000\000\000\000\016\000\020\000\
\020\000\020\000\000\000\020\000\020\000\000\000\016\000\016\000\
\020\000\016\000\000\000\000\000\006\000\016\000\016\000\016\000\
\000\000\016\000\016\000\000\000\006\000\006\000\016\000\006\000\
\000\000\000\000\017\000\006\000\006\000\006\000\000\000\006\000\
\006\000\000\000\017\000\017\000\006\000\017\000\000\000\000\000\
\009\000\017\000\017\000\017\000\000\000\017\000\017\000\000\000\
\009\000\009\000\017\000\009\000\000\000\000\000\007\000\009\000\
\009\000\009\000\000\000\009\000\009\000\000\000\007\000\007\000\
\009\000\007\000\000\000\000\000\008\000\007\000\007\000\007\000\
\000\000\007\000\007\000\000\000\008\000\008\000\007\000\008\000\
\000\000\000\000\000\000\008\000\008\000\008\000\000\000\008\000\
\008\000\004\000\005\000\006\000\008\000\000\000\029\000\030\000\
\031\000\032\000\033\000\034\000\000\000\000\000\000\000\035\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\004\000\
\005\000\006\000\037\000\012\000\029\000\030\000\031\000\032\000\
\033\000\034\000\000\000\046\000\000\000\035\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\004\000\005\000\006\000\
\037\000\012\000\029\000\030\000\031\000\032\000\033\000\034\000\
\000\000\000\000\000\000\035\000\000\000\000\000\000\000\000\000\
\051\000\000\000\000\000\004\000\005\000\006\000\037\000\012\000\
\029\000\030\000\031\000\032\000\033\000\034\000\000\000\000\000\
\000\000\035\000\047\000\000\000\000\000\000\000\000\000\000\000\
\000\000\004\000\005\000\006\000\037\000\012\000\029\000\030\000\
\031\000\032\000\033\000\034\000\000\000\000\000\084\000\035\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\004\000\
\005\000\006\000\037\000\012\000\029\000\030\000\031\000\032\000\
\033\000\034\000\000\000\000\000\000\000\035\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\085\000\
\037\000\012\000\004\000\005\000\006\000\000\000\000\000\029\000\
\030\000\031\000\032\000\033\000\034\000\000\000\000\000\000\000\
\035\000\000\000\000\000\000\000\000\000\000\000\108\000\000\000\
\004\000\005\000\006\000\037\000\012\000\029\000\030\000\031\000\
\032\000\033\000\034\000\000\000\000\000\000\000\035\000\000\000\
\004\000\005\000\006\000\018\000\000\000\000\000\000\000\000\000\
\000\000\037\000\012\000\008\000\000\000\000\000\009\000\000\000\
\010\000\000\000\011\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\012\000"

let yycheck = "\003\001\
\000\000\001\001\002\001\003\001\001\000\002\000\040\000\075\000\
\008\001\009\001\044\000\008\000\009\000\003\001\011\000\015\001\
\028\001\001\001\002\001\003\001\005\001\001\000\002\000\005\001\
\092\000\000\000\005\001\027\001\018\001\063\000\098\000\015\001\
\029\000\030\000\031\000\032\000\033\000\034\000\035\000\024\001\
\037\000\003\001\024\001\027\001\112\000\079\000\043\000\029\001\
\094\000\046\000\000\000\005\001\049\000\025\001\026\001\025\001\
\026\001\001\001\002\001\003\001\018\001\023\001\006\001\007\001\
\008\001\009\001\003\001\014\000\026\001\066\000\024\001\015\001\
\118\000\020\000\071\000\000\000\005\001\024\000\025\000\018\001\
\027\000\115\000\026\001\027\001\081\000\003\001\023\001\084\000\
\016\001\001\001\002\001\003\001\010\001\028\001\091\000\024\001\
\022\001\000\000\095\000\005\001\029\001\026\001\024\001\015\001\
\005\001\052\000\053\000\054\000\055\000\056\000\057\000\058\000\
\059\000\016\001\111\000\027\001\007\000\064\000\000\000\010\000\
\067\000\068\000\003\001\070\000\016\001\029\001\018\001\018\000\
\022\001\010\001\016\001\022\000\022\001\025\001\016\001\026\000\
\083\000\108\000\037\000\000\000\087\000\085\000\112\000\255\255\
\255\255\255\255\255\255\094\000\039\000\096\000\255\255\042\000\
\099\000\000\000\255\255\255\255\255\255\255\255\105\000\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
\255\255\116\000\061\000\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\003\001\255\255\000\000\006\001\007\001\
\008\001\009\001\010\001\011\001\255\255\255\255\255\255\015\001\
\016\001\255\255\255\255\000\000\001\001\002\001\003\001\004\001\
\255\255\025\001\026\001\027\001\255\255\255\255\255\255\012\001\
\255\255\000\000\015\001\102\000\017\001\255\255\019\001\255\255\
\255\255\255\255\109\000\001\001\002\001\003\001\027\001\000\000\
\006\001\007\001\008\001\009\001\010\001\011\001\255\255\255\255\
\255\255\015\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\024\001\255\255\026\001\027\001\255\255\255\255\
\000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\255\255\013\001\014\001\255\255\
\016\001\255\255\255\255\255\255\020\001\021\001\022\001\255\255\
\024\001\025\001\026\001\255\255\255\255\029\001\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\255\255\013\001\014\001\
\255\255\016\001\255\255\255\255\255\255\020\001\021\001\022\001\
\255\255\024\001\025\001\026\001\255\255\255\255\029\001\005\001\
\006\001\007\001\255\255\255\255\010\001\011\001\255\255\013\001\
\014\001\255\255\016\001\255\255\255\255\255\255\020\001\021\001\
\022\001\255\255\024\001\025\001\026\001\255\255\255\255\029\001\
\005\001\006\001\007\001\255\255\255\255\010\001\011\001\255\255\
\013\001\014\001\255\255\016\001\255\255\255\255\255\255\020\001\
\021\001\022\001\255\255\024\001\025\001\026\001\005\001\255\255\
\029\001\255\255\255\255\010\001\011\001\255\255\013\001\014\001\
\255\255\016\001\255\255\255\255\255\255\020\001\021\001\022\001\
\255\255\024\001\025\001\005\001\255\255\255\255\029\001\255\255\
\010\001\011\001\255\255\013\001\014\001\255\255\016\001\255\255\
\255\255\255\255\020\001\021\001\022\001\255\255\024\001\025\001\
\005\001\255\255\255\255\029\001\255\255\010\001\011\001\255\255\
\013\001\014\001\255\255\016\001\255\255\255\255\005\001\020\001\
\021\001\022\001\255\255\024\001\025\001\255\255\013\001\014\001\
\029\001\016\001\255\255\255\255\005\001\020\001\021\001\022\001\
\255\255\024\001\025\001\255\255\013\001\014\001\029\001\016\001\
\255\255\255\255\005\001\020\001\021\001\022\001\255\255\024\001\
\025\001\255\255\013\001\014\001\029\001\016\001\255\255\255\255\
\005\001\020\001\021\001\022\001\255\255\024\001\025\001\255\255\
\013\001\014\001\029\001\016\001\255\255\255\255\005\001\020\001\
\021\001\022\001\255\255\024\001\025\001\255\255\013\001\014\001\
\029\001\016\001\255\255\255\255\005\001\020\001\021\001\022\001\
\255\255\024\001\025\001\255\255\013\001\014\001\029\001\016\001\
\255\255\255\255\255\255\020\001\021\001\022\001\255\255\024\001\
\025\001\001\001\002\001\003\001\029\001\255\255\006\001\007\001\
\008\001\009\001\010\001\011\001\255\255\255\255\255\255\015\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\001\001\
\002\001\003\001\026\001\027\001\006\001\007\001\008\001\009\001\
\010\001\011\001\255\255\013\001\255\255\015\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\001\001\002\001\003\001\
\026\001\027\001\006\001\007\001\008\001\009\001\010\001\011\001\
\255\255\255\255\255\255\015\001\255\255\255\255\255\255\255\255\
\020\001\255\255\255\255\001\001\002\001\003\001\026\001\027\001\
\006\001\007\001\008\001\009\001\010\001\011\001\255\255\255\255\
\255\255\015\001\016\001\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\003\001\026\001\027\001\006\001\007\001\
\008\001\009\001\010\001\011\001\255\255\255\255\014\001\015\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\001\001\
\002\001\003\001\026\001\027\001\006\001\007\001\008\001\009\001\
\010\001\011\001\255\255\255\255\255\255\015\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\025\001\
\026\001\027\001\001\001\002\001\003\001\255\255\255\255\006\001\
\007\001\008\001\009\001\010\001\011\001\255\255\255\255\255\255\
\015\001\255\255\255\255\255\255\255\255\255\255\021\001\255\255\
\001\001\002\001\003\001\026\001\027\001\006\001\007\001\008\001\
\009\001\010\001\011\001\255\255\255\255\255\255\015\001\255\255\
\001\001\002\001\003\001\004\001\255\255\255\255\255\255\255\255\
\255\255\026\001\027\001\012\001\255\255\255\255\015\001\255\255\
\017\001\255\255\019\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\027\001"

let yynames_const = "\
  LET\000\
  IN\000\
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
  FUN\000\
  ARROW\000\
  MATCH\000\
  WITH\000\
  OR\000\
  END\000\
  REC\000\
  SEMI\000\
  EOF\000\
  COMMA\000\
  CONS\000\
  LBRACKET\000\
  RBRACKET\000\
  AND\000\
  "

let yynames_block = "\
  INT\000\
  BOOL\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 45 "ex4Parser.mly"
           ( _1 )
# 378 "ex4Parser.ml"
               : Ex4Syntax.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 49 "ex4Parser.mly"
              ( CExp _1 )
# 385 "ex4Parser.ml"
               : Ex4Syntax.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'var_expr) in
    Obj.repr(
# 50 "ex4Parser.mly"
                          ( CLet (_2, _3) )
# 393 "ex4Parser.ml"
               : Ex4Syntax.command))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'var_expr) in
    Obj.repr(
# 51 "ex4Parser.mly"
                                  ( CRecFun (_3, _4, _5) )
# 402 "ex4Parser.ml"
               : Ex4Syntax.command))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'var_expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'and_expr) in
    Obj.repr(
# 52 "ex4Parser.mly"
                                           ( CRecFunand ((_3, _4, _5) :: _6) )
# 412 "ex4Parser.ml"
               : Ex4Syntax.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'var_expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 56 "ex4Parser.mly"
                                  ( ELet(_2,_3,_5) )
# 421 "ex4Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'var_expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 57 "ex4Parser.mly"
                                      ( ERecFun (_3, _4, _5, _7) )
# 431 "ex4Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'var) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'and_expr) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 58 "ex4Parser.mly"
                                             ( ERecFunand (((_3,_4,_6) :: _7), _9) )
# 442 "ex4Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 59 "ex4Parser.mly"
                                 ( EIf(_2,_4,_6) )
# 451 "ex4Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 60 "ex4Parser.mly"
                 ( EEqual(_1, _3) )
# 459 "ex4Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 61 "ex4Parser.mly"
                 ( ECompare(_1, _3) )
# 467 "ex4Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 62 "ex4Parser.mly"
                    ( EBin(OpAdd, _1, _3) )
# 475 "ex4Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 63 "ex4Parser.mly"
                    ( EBin(OpSub, _1, _3) )
# 483 "ex4Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 64 "ex4Parser.mly"
                    ( EBin(OpMul, _1, _3) )
# 491 "ex4Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 65 "ex4Parser.mly"
                     ( EBin(OpDiv, _1, _3) )
# 499 "ex4Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 66 "ex4Parser.mly"
                       ( EFun(_2,_4) )
# 507 "ex4Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 67 "ex4Parser.mly"
                           ( EFun(_2, EFun(_3, _5)) )
# 516 "ex4Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'apply_expr) in
    Obj.repr(
# 68 "ex4Parser.mly"
                       (EApp (_1,_2))
# 524 "ex4Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'pattern_expr) in
    Obj.repr(
# 69 "ex4Parser.mly"
                                     ( EMatch (_2, _4) )
# 532 "ex4Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 70 "ex4Parser.mly"
                   ( ECons (_1, _3) )
# 540 "ex4Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cons_expr) in
    Obj.repr(
# 71 "ex4Parser.mly"
               ( _1 )
# 547 "ex4Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'apply_expr) in
    Obj.repr(
# 72 "ex4Parser.mly"
               ( _1 )
# 554 "ex4Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 76 "ex4Parser.mly"
            ( _2 )
# 561 "ex4Parser.ml"
               : 'var_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'var) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'var_expr) in
    Obj.repr(
# 77 "ex4Parser.mly"
                 ( EFun (_1, _2) )
# 569 "ex4Parser.ml"
               : 'var_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'var_expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'and_expr) in
    Obj.repr(
# 81 "ex4Parser.mly"
                                   ( (_2,_3,_4) :: _5 )
# 579 "ex4Parser.ml"
               : 'and_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "ex4Parser.mly"
                            ( [] )
# 585 "ex4Parser.ml"
               : 'and_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'noncons_expr) in
    Obj.repr(
# 86 "ex4Parser.mly"
                                     ( _1 )
# 592 "ex4Parser.ml"
               : 'cons_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cons_expr) in
    Obj.repr(
# 87 "ex4Parser.mly"
                                     ( ECons (_1, _3) )
# 600 "ex4Parser.ml"
               : 'cons_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'apply_expr) in
    Obj.repr(
# 91 "ex4Parser.mly"
                                     ( _1 )
# 607 "ex4Parser.ml"
               : 'noncons_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list) in
    Obj.repr(
# 92 "ex4Parser.mly"
                                     ( ETuple (_2 :: _4) )
# 615 "ex4Parser.ml"
               : 'noncons_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 96 "ex4Parser.mly"
         ( [_1] )
# 622 "ex4Parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_list) in
    Obj.repr(
# 97 "ex4Parser.mly"
                         ( _1 :: _3 )
# 630 "ex4Parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 102 "ex4Parser.mly"
    ( [] )
# 636 "ex4Parser.ml"
               : 'pattern_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 103 "ex4Parser.mly"
                        ( [(_1, _3)] )
# 644 "ex4Parser.ml"
               : 'pattern_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'pattern_expr) in
    Obj.repr(
# 104 "ex4Parser.mly"
                                       ( (_1, _3) :: _5 )
# 653 "ex4Parser.ml"
               : 'pattern_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 108 "ex4Parser.mly"
            ( PInt _1 )
# 660 "ex4Parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 109 "ex4Parser.mly"
            ( PBool _1 )
# 667 "ex4Parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 110 "ex4Parser.mly"
            ( PVar _1 )
# 674 "ex4Parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    Obj.repr(
# 111 "ex4Parser.mly"
                      ( PNil )
# 680 "ex4Parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'pattern) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'pattern_list) in
    Obj.repr(
# 112 "ex4Parser.mly"
                                         ( PTuple (_2 :: _4) )
# 688 "ex4Parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pattern) in
    Obj.repr(
# 113 "ex4Parser.mly"
                         ( PCons (_1, _3) )
# 696 "ex4Parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'pattern) in
    Obj.repr(
# 117 "ex4Parser.mly"
            ( [_1] )
# 703 "ex4Parser.ml"
               : 'pattern_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pattern_list) in
    Obj.repr(
# 118 "ex4Parser.mly"
                               ( _1 :: _3 )
# 711 "ex4Parser.ml"
               : 'pattern_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 122 "ex4Parser.mly"
                    ( EValue (VInt _1) )
# 718 "ex4Parser.ml"
               : 'apply_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 123 "ex4Parser.mly"
                    ( EValue (VBool _1) )
# 725 "ex4Parser.ml"
               : 'apply_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 124 "ex4Parser.mly"
                    ( EVar _1 )
# 732 "ex4Parser.ml"
               : 'apply_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 125 "ex4Parser.mly"
                    ( _2 )
# 739 "ex4Parser.ml"
               : 'apply_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 126 "ex4Parser.mly"
                       ( EValue VNil  )
# 745 "ex4Parser.ml"
               : 'apply_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 132 "ex4Parser.mly"
       ( _1 )
# 752 "ex4Parser.ml"
               : 'var))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry command *)
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ex4Syntax.expr)
let command (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : Ex4Syntax.command)
