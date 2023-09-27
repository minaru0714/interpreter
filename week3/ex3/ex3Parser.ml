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
# 2 "ex3Parser.mly"
  open Ex3Syntax
# 38 "ex3Parser.ml"
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
\001\000\002\000\002\000\002\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\005\000\005\000\006\000\006\000\
\009\000\009\000\010\000\010\000\011\000\011\000\008\000\008\000\
\012\000\012\000\012\000\012\000\012\000\012\000\013\000\013\000\
\007\000\007\000\007\000\007\000\007\000\004\000\000\000\000\000"

let yylen = "\002\000\
\002\000\002\000\005\000\006\000\006\000\007\000\008\000\006\000\
\003\000\003\000\003\000\003\000\003\000\003\000\004\000\002\000\
\005\000\003\000\001\000\001\000\002\000\002\000\006\000\000\000\
\001\000\003\000\001\000\005\000\001\000\003\000\003\000\005\000\
\001\000\001\000\001\000\002\000\005\000\003\000\001\000\003\000\
\001\000\001\000\001\000\003\000\002\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\041\000\042\000\043\000\000\000\000\000\
\000\000\000\000\000\000\000\000\047\000\000\000\020\000\019\000\
\025\000\000\000\048\000\000\000\046\000\000\000\000\000\000\000\
\000\000\000\000\000\000\045\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\001\000\000\000\016\000\000\000\000\000\
\002\000\000\000\000\000\000\000\044\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\019\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\033\000\034\000\035\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\028\000\000\000\036\000\017\000\000\000\000\000\000\000\003\000\
\000\000\022\000\000\000\000\000\000\000\000\000\000\000\030\000\
\000\000\000\000\000\000\004\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\037\000\032\000\000\000\
\040\000\000\000\023\000"

let yydgoto = "\003\000\
\013\000\019\000\063\000\076\000\077\000\093\000\015\000\071\000\
\016\000\017\000\064\000\072\000\105\000"

let yysindex = "\069\000\
\226\255\196\001\000\000\000\000\000\000\000\000\004\255\226\255\
\226\255\009\255\226\255\246\254\000\000\235\000\000\000\000\000\
\000\000\020\255\000\000\210\255\000\000\009\255\038\255\001\001\
\057\255\034\255\023\001\000\000\226\255\226\255\226\255\226\255\
\226\255\226\255\226\255\000\000\226\255\000\000\009\255\046\255\
\000\000\009\255\226\255\226\255\000\000\226\255\226\255\013\255\
\061\001\061\001\105\255\105\255\180\001\180\001\045\001\180\001\
\000\000\009\255\226\255\044\255\072\001\094\001\116\001\059\255\
\165\001\000\000\000\000\000\000\013\255\062\255\072\255\238\254\
\044\255\182\255\226\255\044\255\015\255\226\255\226\255\226\255\
\000\000\053\255\000\000\000\000\226\255\013\255\000\255\000\000\
\165\001\000\000\226\255\009\255\080\255\165\001\165\001\000\000\
\013\255\143\001\069\255\000\000\165\001\009\255\226\255\063\255\
\084\255\013\255\092\255\165\001\013\255\000\000\000\000\226\255\
\000\000\149\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\051\000\076\000\001\000\026\000\098\000\119\000\000\000\140\000\
\000\000\000\000\000\000\000\000\000\000\000\000\087\255\000\000\
\154\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\045\255\000\000\000\000\000\000\000\000\168\000\182\000\000\000\
\000\000\082\255\037\255\000\000\196\000\000\000\000\000\093\255\
\000\000\000\000\000\000\210\000\000\000\000\000\000\000\000\000\
\000\000\106\255\000\000"

let yygindex = "\000\000\
\000\000\000\000\002\000\255\255\205\255\252\255\072\000\006\000\
\078\000\000\000\033\000\189\255\007\000"

let yytablesize = 735
let yytable = "\085\000\
\013\000\082\000\014\000\020\000\091\000\023\000\021\000\086\000\
\026\000\024\000\025\000\021\000\027\000\066\000\067\000\068\000\
\040\000\028\000\099\000\091\000\042\000\087\000\021\000\100\000\
\090\000\014\000\022\000\069\000\092\000\104\000\049\000\050\000\
\051\000\052\000\053\000\054\000\055\000\058\000\056\000\070\000\
\060\000\104\000\039\000\092\000\061\000\062\000\021\000\043\000\
\065\000\021\000\011\000\047\000\038\000\075\000\038\000\059\000\
\073\000\004\000\005\000\006\000\074\000\038\000\029\000\030\000\
\031\000\032\000\033\000\034\000\021\000\001\000\002\000\035\000\
\045\000\021\000\081\000\012\000\089\000\097\000\086\000\094\000\
\095\000\046\000\037\000\012\000\103\000\038\000\098\000\109\000\
\086\000\083\000\102\000\038\000\101\000\084\000\086\000\038\000\
\038\000\009\000\038\000\110\000\107\000\112\000\029\000\031\000\
\108\000\004\000\005\000\006\000\039\000\115\000\024\000\111\000\
\096\000\114\000\057\000\113\000\000\000\000\000\010\000\035\000\
\038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
\000\000\000\000\000\000\012\000\038\000\038\000\038\000\000\000\
\038\000\000\000\000\000\018\000\000\000\000\000\000\000\000\000\
\000\000\038\000\000\000\000\000\000\000\004\000\005\000\006\000\
\000\000\015\000\029\000\030\000\031\000\032\000\033\000\034\000\
\038\000\000\000\000\000\035\000\000\000\038\000\038\000\005\000\
\000\000\038\000\000\000\000\000\038\000\000\000\037\000\012\000\
\000\000\092\000\000\000\038\000\000\000\008\000\004\000\005\000\
\006\000\038\000\078\000\029\000\030\000\031\000\032\000\033\000\
\034\000\000\000\000\000\006\000\035\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\088\000\000\000\037\000\
\012\000\007\000\004\000\005\000\006\000\000\000\000\000\029\000\
\030\000\031\000\032\000\033\000\034\000\000\000\000\000\000\000\
\035\000\000\000\004\000\005\000\006\000\007\000\000\000\000\000\
\000\000\041\000\036\000\037\000\012\000\008\000\000\000\000\000\
\009\000\000\000\010\000\000\000\011\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\012\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\000\000\013\000\013\000\000\000\
\013\000\000\000\000\000\000\000\013\000\013\000\013\000\000\000\
\013\000\013\000\013\000\000\000\000\000\013\000\014\000\014\000\
\014\000\014\000\014\000\014\000\014\000\000\000\014\000\014\000\
\000\000\014\000\000\000\000\000\000\000\014\000\014\000\014\000\
\000\000\014\000\014\000\014\000\000\000\000\000\014\000\011\000\
\011\000\011\000\000\000\000\000\011\000\011\000\000\000\011\000\
\011\000\000\000\011\000\000\000\000\000\000\000\011\000\011\000\
\011\000\000\000\011\000\011\000\011\000\000\000\000\000\011\000\
\012\000\012\000\012\000\000\000\000\000\012\000\012\000\000\000\
\012\000\012\000\000\000\012\000\000\000\000\000\000\000\012\000\
\012\000\012\000\000\000\012\000\012\000\012\000\009\000\000\000\
\012\000\000\000\000\000\009\000\009\000\000\000\009\000\009\000\
\000\000\009\000\000\000\000\000\000\000\009\000\009\000\009\000\
\000\000\009\000\009\000\010\000\000\000\000\000\009\000\000\000\
\010\000\010\000\000\000\010\000\010\000\000\000\010\000\000\000\
\000\000\000\000\010\000\010\000\010\000\000\000\010\000\010\000\
\018\000\000\000\000\000\010\000\000\000\018\000\018\000\000\000\
\018\000\018\000\000\000\018\000\000\000\000\000\015\000\018\000\
\018\000\018\000\000\000\018\000\018\000\000\000\015\000\015\000\
\018\000\015\000\000\000\000\000\005\000\015\000\015\000\015\000\
\000\000\015\000\015\000\000\000\005\000\005\000\015\000\005\000\
\000\000\000\000\008\000\005\000\005\000\005\000\000\000\005\000\
\005\000\000\000\008\000\008\000\005\000\008\000\000\000\000\000\
\006\000\008\000\008\000\008\000\000\000\008\000\008\000\000\000\
\006\000\006\000\008\000\006\000\000\000\000\000\007\000\006\000\
\006\000\006\000\000\000\006\000\006\000\000\000\007\000\007\000\
\006\000\007\000\000\000\000\000\000\000\007\000\007\000\007\000\
\000\000\007\000\007\000\004\000\005\000\006\000\007\000\000\000\
\029\000\030\000\031\000\032\000\033\000\034\000\000\000\000\000\
\000\000\035\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\004\000\005\000\006\000\037\000\012\000\029\000\030\000\
\031\000\032\000\033\000\034\000\000\000\044\000\000\000\035\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\004\000\
\005\000\006\000\037\000\012\000\029\000\030\000\031\000\032\000\
\033\000\034\000\000\000\000\000\000\000\035\000\000\000\000\000\
\000\000\000\000\048\000\000\000\000\000\004\000\005\000\006\000\
\037\000\012\000\029\000\030\000\031\000\032\000\033\000\034\000\
\000\000\000\000\000\000\035\000\045\000\004\000\005\000\006\000\
\000\000\000\000\000\000\000\000\031\000\032\000\037\000\012\000\
\004\000\005\000\006\000\035\000\078\000\029\000\030\000\031\000\
\032\000\033\000\034\000\000\000\000\000\000\000\035\000\012\000\
\000\000\000\000\000\000\000\000\000\000\000\000\004\000\005\000\
\006\000\037\000\012\000\029\000\030\000\031\000\032\000\033\000\
\034\000\000\000\000\000\079\000\035\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\004\000\005\000\006\000\037\000\
\012\000\029\000\030\000\031\000\032\000\033\000\034\000\000\000\
\000\000\000\000\035\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\080\000\037\000\012\000\004\000\
\005\000\006\000\000\000\000\000\029\000\030\000\031\000\032\000\
\033\000\034\000\000\000\000\000\000\000\035\000\000\000\000\000\
\000\000\000\000\000\000\106\000\000\000\004\000\005\000\006\000\
\037\000\012\000\029\000\030\000\031\000\032\000\033\000\034\000\
\000\000\000\000\000\000\035\000\004\000\005\000\006\000\000\000\
\000\000\029\000\030\000\031\000\032\000\000\000\037\000\012\000\
\000\000\000\000\035\000\000\000\004\000\005\000\006\000\018\000\
\000\000\000\000\000\000\000\000\000\000\037\000\012\000\008\000\
\000\000\000\000\009\000\000\000\010\000\000\000\011\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\012\000"

let yycheck = "\018\001\
\000\000\069\000\001\000\002\000\005\001\007\000\003\001\026\001\
\010\000\008\000\009\000\003\001\011\000\001\001\002\001\003\001\
\018\000\028\001\086\000\005\001\022\000\073\000\003\001\024\001\
\076\000\000\000\023\001\015\001\029\001\097\000\029\000\030\000\
\031\000\032\000\033\000\034\000\035\000\039\000\037\000\027\001\
\042\000\109\000\023\001\029\001\043\000\044\000\003\001\010\001\
\047\000\005\001\000\000\018\001\016\001\010\001\018\001\010\001\
\058\000\001\001\002\001\003\001\059\000\025\001\006\001\007\001\
\008\001\009\001\010\001\011\001\024\001\001\000\002\000\015\001\
\016\001\029\001\016\001\000\000\075\000\025\001\026\001\078\000\
\079\000\025\001\026\001\027\001\005\001\014\000\085\000\025\001\
\026\001\028\001\092\000\020\000\091\000\022\001\026\001\024\000\
\025\000\000\000\027\000\016\001\102\000\010\001\016\001\022\001\
\103\000\001\001\002\001\003\001\016\001\114\000\005\001\106\000\
\080\000\112\000\037\000\109\000\255\255\255\255\000\000\015\001\
\049\000\050\000\051\000\052\000\053\000\054\000\055\000\056\000\
\255\255\255\255\255\255\027\001\061\000\062\000\063\000\255\255\
\065\000\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
\255\255\074\000\255\255\255\255\255\255\001\001\002\001\003\001\
\255\255\000\000\006\001\007\001\008\001\009\001\010\001\011\001\
\089\000\255\255\255\255\015\001\255\255\094\000\095\000\000\000\
\255\255\098\000\255\255\255\255\101\000\255\255\026\001\027\001\
\255\255\029\001\255\255\108\000\255\255\000\000\001\001\002\001\
\003\001\114\000\005\001\006\001\007\001\008\001\009\001\010\001\
\011\001\255\255\255\255\000\000\015\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\024\001\255\255\026\001\
\027\001\000\000\001\001\002\001\003\001\255\255\255\255\006\001\
\007\001\008\001\009\001\010\001\011\001\255\255\255\255\255\255\
\015\001\255\255\001\001\002\001\003\001\004\001\255\255\255\255\
\255\255\024\001\000\000\026\001\027\001\012\001\255\255\255\255\
\015\001\255\255\017\001\255\255\019\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\027\001\255\255\255\255\255\255\
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
\029\001\016\001\255\255\255\255\255\255\020\001\021\001\022\001\
\255\255\024\001\025\001\001\001\002\001\003\001\029\001\255\255\
\006\001\007\001\008\001\009\001\010\001\011\001\255\255\255\255\
\255\255\015\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\003\001\026\001\027\001\006\001\007\001\
\008\001\009\001\010\001\011\001\255\255\013\001\255\255\015\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\001\001\
\002\001\003\001\026\001\027\001\006\001\007\001\008\001\009\001\
\010\001\011\001\255\255\255\255\255\255\015\001\255\255\255\255\
\255\255\255\255\020\001\255\255\255\255\001\001\002\001\003\001\
\026\001\027\001\006\001\007\001\008\001\009\001\010\001\011\001\
\255\255\255\255\255\255\015\001\016\001\001\001\002\001\003\001\
\255\255\255\255\255\255\255\255\008\001\009\001\026\001\027\001\
\001\001\002\001\003\001\015\001\005\001\006\001\007\001\008\001\
\009\001\010\001\011\001\255\255\255\255\255\255\015\001\027\001\
\255\255\255\255\255\255\255\255\255\255\255\255\001\001\002\001\
\003\001\026\001\027\001\006\001\007\001\008\001\009\001\010\001\
\011\001\255\255\255\255\014\001\015\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\001\001\002\001\003\001\026\001\
\027\001\006\001\007\001\008\001\009\001\010\001\011\001\255\255\
\255\255\255\255\015\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\025\001\026\001\027\001\001\001\
\002\001\003\001\255\255\255\255\006\001\007\001\008\001\009\001\
\010\001\011\001\255\255\255\255\255\255\015\001\255\255\255\255\
\255\255\255\255\255\255\021\001\255\255\001\001\002\001\003\001\
\026\001\027\001\006\001\007\001\008\001\009\001\010\001\011\001\
\255\255\255\255\255\255\015\001\001\001\002\001\003\001\255\255\
\255\255\006\001\007\001\008\001\009\001\255\255\026\001\027\001\
\255\255\255\255\015\001\255\255\001\001\002\001\003\001\004\001\
\255\255\255\255\255\255\255\255\255\255\026\001\027\001\012\001\
\255\255\255\255\015\001\255\255\017\001\255\255\019\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\027\001"

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
# 45 "ex3Parser.mly"
           ( _1 )
# 382 "ex3Parser.ml"
               : Ex3Syntax.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 49 "ex3Parser.mly"
              ( CExp _1 )
# 389 "ex3Parser.ml"
               : Ex3Syntax.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 50 "ex3Parser.mly"
                         ( CLet (_2, _4) )
# 397 "ex3Parser.ml"
               : Ex3Syntax.command))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'var_expr) in
    Obj.repr(
# 51 "ex3Parser.mly"
                                  ( CRecFun (_3, _4, _5) )
# 406 "ex3Parser.ml"
               : Ex3Syntax.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 55 "ex3Parser.mly"
                                 ( ELet(_2,_4,_6) )
# 415 "ex3Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'var_expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 56 "ex3Parser.mly"
                                      ( ERecFun (_3, _4, _5, _7) )
# 425 "ex3Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'var_expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'and_expr) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 57 "ex3Parser.mly"
                                               ( ERecFunand (((_3,_4,_5) :: _6), _8) )
# 436 "ex3Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 58 "ex3Parser.mly"
                                 ( EIf(_2,_4,_6) )
# 445 "ex3Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 59 "ex3Parser.mly"
                 ( EEqual(_1, _3) )
# 453 "ex3Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 60 "ex3Parser.mly"
                 ( ECompare(_1, _3) )
# 461 "ex3Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 61 "ex3Parser.mly"
                    ( EBin(OpAdd, _1, _3) )
# 469 "ex3Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 62 "ex3Parser.mly"
                    ( EBin(OpSub, _1, _3) )
# 477 "ex3Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 63 "ex3Parser.mly"
                    ( EBin(OpMul, _1, _3) )
# 485 "ex3Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 64 "ex3Parser.mly"
                     ( EBin(OpDiv, _1, _3) )
# 493 "ex3Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 65 "ex3Parser.mly"
                       ( EFun(_2,_4) )
# 501 "ex3Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'apply_expr) in
    Obj.repr(
# 66 "ex3Parser.mly"
                       (EApp (_1,_2))
# 509 "ex3Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'pattern_expr) in
    Obj.repr(
# 67 "ex3Parser.mly"
                                     ( EMatch (_2, _4) )
# 517 "ex3Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 68 "ex3Parser.mly"
                   ( ECons (_1, _3) )
# 525 "ex3Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cons_expr) in
    Obj.repr(
# 69 "ex3Parser.mly"
               ( _1 )
# 532 "ex3Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'apply_expr) in
    Obj.repr(
# 70 "ex3Parser.mly"
               ( _1 )
# 539 "ex3Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 75 "ex3Parser.mly"
            ( _2 )
# 546 "ex3Parser.ml"
               : 'var_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'var) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'var_expr) in
    Obj.repr(
# 76 "ex3Parser.mly"
                 ( EFun (_1, _2) )
# 554 "ex3Parser.ml"
               : 'var_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'and_expr) in
    Obj.repr(
# 81 "ex3Parser.mly"
                                  ( (_2,_3,_5) :: _6 )
# 564 "ex3Parser.ml"
               : 'and_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "ex3Parser.mly"
                            ( [] )
# 570 "ex3Parser.ml"
               : 'and_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'noncons_expr) in
    Obj.repr(
# 86 "ex3Parser.mly"
                                     ( _1 )
# 577 "ex3Parser.ml"
               : 'cons_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cons_expr) in
    Obj.repr(
# 87 "ex3Parser.mly"
                                     ( ECons (_1, _3) )
# 585 "ex3Parser.ml"
               : 'cons_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'apply_expr) in
    Obj.repr(
# 91 "ex3Parser.mly"
                                     ( _1 )
# 592 "ex3Parser.ml"
               : 'noncons_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list) in
    Obj.repr(
# 92 "ex3Parser.mly"
                                     ( ETuple (_2 :: _4) )
# 600 "ex3Parser.ml"
               : 'noncons_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 96 "ex3Parser.mly"
         ( [_1] )
# 607 "ex3Parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_list) in
    Obj.repr(
# 97 "ex3Parser.mly"
                         ( _1 :: _3 )
# 615 "ex3Parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "ex3Parser.mly"
                        ( [(_1, _3)] )
# 623 "ex3Parser.ml"
               : 'pattern_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'pattern_expr) in
    Obj.repr(
# 103 "ex3Parser.mly"
                                       ( (_1, _3) :: _5 )
# 632 "ex3Parser.ml"
               : 'pattern_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 107 "ex3Parser.mly"
            ( PInt _1 )
# 639 "ex3Parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 108 "ex3Parser.mly"
            ( PBool _1 )
# 646 "ex3Parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 109 "ex3Parser.mly"
            ( PVar _1 )
# 653 "ex3Parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    Obj.repr(
# 110 "ex3Parser.mly"
                      ( PNil )
# 659 "ex3Parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'pattern) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'pattern_list) in
    Obj.repr(
# 111 "ex3Parser.mly"
                                         ( PTuple (_2 :: _4) )
# 667 "ex3Parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pattern) in
    Obj.repr(
# 112 "ex3Parser.mly"
                         ( PCons (_1, _3) )
# 675 "ex3Parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'pattern) in
    Obj.repr(
# 116 "ex3Parser.mly"
            ( [_1] )
# 682 "ex3Parser.ml"
               : 'pattern_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pattern_list) in
    Obj.repr(
# 117 "ex3Parser.mly"
                               ( _1 :: _3 )
# 690 "ex3Parser.ml"
               : 'pattern_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 121 "ex3Parser.mly"
                    ( EValue (VInt _1) )
# 697 "ex3Parser.ml"
               : 'apply_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 122 "ex3Parser.mly"
                    ( EValue (VBool _1) )
# 704 "ex3Parser.ml"
               : 'apply_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 123 "ex3Parser.mly"
                    ( EVar _1 )
# 711 "ex3Parser.ml"
               : 'apply_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 124 "ex3Parser.mly"
                    ( _2 )
# 718 "ex3Parser.ml"
               : 'apply_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 125 "ex3Parser.mly"
                       ( ENil )
# 724 "ex3Parser.ml"
               : 'apply_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 131 "ex3Parser.mly"
       ( _1 )
# 731 "ex3Parser.ml"
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ex3Syntax.expr)
let command (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : Ex3Syntax.command)
