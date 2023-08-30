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
# 2 "ex1Parser.mly"
  open Ex1Syntax
# 38 "ex1Parser.ml"
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
\001\000\001\000\002\000\002\000\002\000\002\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\005\000\005\000\008\000\
\008\000\007\000\007\000\009\000\009\000\009\000\009\000\009\000\
\009\000\010\000\010\000\006\000\006\000\006\000\006\000\006\000\
\004\000\000\000\000\000"

let yylen = "\002\000\
\002\000\002\000\002\000\005\000\009\000\006\000\006\000\008\000\
\006\000\006\000\003\000\003\000\003\000\003\000\003\000\003\000\
\004\000\002\000\005\000\003\000\001\000\006\000\008\000\001\000\
\003\000\003\000\005\000\001\000\001\000\001\000\002\000\005\000\
\003\000\001\000\003\000\001\000\001\000\001\000\003\000\002\000\
\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\036\000\037\000\038\000\000\000\000\000\
\000\000\000\000\000\000\000\000\042\000\000\000\021\000\000\000\
\043\000\000\000\041\000\000\000\000\000\000\000\000\000\000\000\
\000\000\040\000\000\000\000\000\000\000\000\000\000\000\000\000\
\002\000\001\000\000\000\018\000\000\000\000\000\003\000\000\000\
\000\000\000\000\000\000\039\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\028\000\029\000\
\030\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\031\000\019\000\
\000\000\000\000\000\000\000\000\004\000\000\000\000\000\009\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\032\000\027\000\000\000\000\000\035\000\005\000\000\000\023\000"

let yydgoto = "\003\000\
\013\000\017\000\014\000\058\000\041\000\015\000\068\000\000\000\
\069\000\099\000"

let yysindex = "\027\000\
\045\255\037\002\000\000\000\000\000\000\000\000\000\255\045\255\
\045\255\005\255\045\255\238\254\000\000\212\000\000\000\001\255\
\000\000\037\001\000\000\005\255\004\255\064\001\086\001\012\255\
\108\001\000\000\045\255\045\255\045\255\045\255\045\255\045\255\
\000\000\000\000\045\255\000\000\005\255\023\255\000\000\010\255\
\030\255\045\255\045\255\000\000\045\255\097\255\124\001\124\001\
\107\255\107\255\234\255\234\255\234\255\024\255\031\255\045\255\
\026\255\040\255\045\255\135\001\157\001\021\002\000\000\000\000\
\000\000\097\255\028\255\032\255\255\254\044\255\045\255\010\001\
\005\255\045\255\179\001\045\255\045\255\033\255\000\000\000\000\
\045\255\097\255\005\255\206\001\000\000\047\255\233\001\000\000\
\021\002\021\002\097\255\255\001\037\255\049\255\000\000\045\255\
\045\255\048\255\054\255\097\255\045\255\218\255\021\002\097\255\
\000\000\000\000\189\255\005\255\000\000\000\000\065\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\051\000\076\000\
\001\000\026\000\096\000\116\000\136\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\148\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\162\000\174\000\000\000\055\255\247\254\000\000\239\000\000\000\
\000\000\063\255\000\000\000\000\000\000\075\255\188\000\000\000\
\000\000\000\000\075\255\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\010\000\251\255\219\255\079\000\238\255\000\000\
\196\255\235\255"

let yytablesize = 832
let yytable = "\055\000\
\015\000\021\000\019\000\019\000\024\000\078\000\033\000\019\000\
\033\000\026\000\038\000\018\000\019\000\042\000\040\000\033\000\
\081\000\022\000\023\000\057\000\025\000\093\000\020\000\037\000\
\082\000\016\000\019\000\001\000\002\000\045\000\098\000\054\000\
\056\000\070\000\059\000\071\000\047\000\048\000\049\000\050\000\
\051\000\052\000\073\000\098\000\053\000\004\000\005\000\006\000\
\007\000\074\000\013\000\060\000\061\000\080\000\062\000\079\000\
\008\000\091\000\082\000\009\000\083\000\010\000\082\000\011\000\
\096\000\072\000\101\000\086\000\075\000\105\000\112\000\012\000\
\104\000\082\000\057\000\014\000\026\000\094\000\034\000\022\000\
\084\000\106\000\109\000\087\000\000\000\089\000\090\000\000\000\
\000\000\000\000\092\000\000\000\036\000\000\000\000\000\011\000\
\036\000\063\000\064\000\065\000\036\000\036\000\111\000\036\000\
\000\000\102\000\103\000\004\000\005\000\006\000\107\000\066\000\
\000\000\000\000\000\000\012\000\000\000\000\000\000\000\000\000\
\000\000\009\000\000\000\067\000\000\000\036\000\036\000\036\000\
\036\000\036\000\036\000\036\000\000\000\012\000\000\000\020\000\
\000\000\000\000\036\000\036\000\036\000\000\000\000\000\000\000\
\000\000\000\000\000\000\017\000\000\000\000\000\036\000\000\000\
\000\000\036\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\007\000\036\000\000\000\000\000\036\000\000\000\036\000\
\036\000\000\000\036\000\000\000\000\000\010\000\000\000\000\000\
\000\000\000\000\000\000\000\000\036\000\036\000\000\000\000\000\
\000\000\036\000\000\000\008\000\000\000\004\000\005\000\006\000\
\000\000\000\000\027\000\028\000\029\000\030\000\031\000\032\000\
\000\000\000\000\000\000\009\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\034\000\110\000\000\000\035\000\012\000\
\000\000\108\000\004\000\005\000\006\000\000\000\000\000\027\000\
\028\000\029\000\030\000\031\000\032\000\000\000\000\000\000\000\
\009\000\000\000\004\000\005\000\006\000\000\000\006\000\027\000\
\028\000\029\000\030\000\035\000\012\000\000\000\108\000\000\000\
\009\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\035\000\012\000\015\000\015\000\015\000\
\015\000\015\000\015\000\015\000\000\000\015\000\015\000\000\000\
\015\000\000\000\000\000\000\000\015\000\015\000\015\000\000\000\
\015\000\000\000\015\000\000\000\000\000\015\000\016\000\016\000\
\016\000\016\000\016\000\016\000\016\000\000\000\016\000\016\000\
\000\000\016\000\000\000\000\000\000\000\016\000\016\000\016\000\
\000\000\016\000\000\000\016\000\000\000\000\000\016\000\013\000\
\013\000\013\000\000\000\000\000\013\000\013\000\000\000\013\000\
\013\000\000\000\013\000\000\000\000\000\000\000\013\000\013\000\
\013\000\000\000\013\000\000\000\013\000\000\000\000\000\013\000\
\014\000\014\000\014\000\000\000\000\000\014\000\014\000\000\000\
\014\000\014\000\000\000\014\000\000\000\000\000\000\000\014\000\
\014\000\014\000\000\000\014\000\011\000\014\000\000\000\000\000\
\014\000\011\000\011\000\000\000\011\000\011\000\000\000\011\000\
\000\000\000\000\000\000\011\000\011\000\011\000\000\000\011\000\
\012\000\000\000\000\000\000\000\011\000\012\000\012\000\000\000\
\012\000\012\000\000\000\012\000\000\000\000\000\000\000\012\000\
\012\000\012\000\000\000\012\000\020\000\000\000\000\000\000\000\
\012\000\020\000\020\000\000\000\020\000\020\000\000\000\020\000\
\017\000\000\000\000\000\020\000\020\000\020\000\000\000\020\000\
\017\000\017\000\000\000\017\000\020\000\000\000\007\000\017\000\
\017\000\017\000\000\000\017\000\000\000\000\000\007\000\007\000\
\017\000\007\000\010\000\000\000\000\000\007\000\007\000\007\000\
\000\000\007\000\010\000\010\000\000\000\010\000\007\000\000\000\
\008\000\010\000\010\000\010\000\000\000\010\000\000\000\000\000\
\008\000\008\000\010\000\008\000\000\000\000\000\000\000\008\000\
\008\000\008\000\000\000\008\000\004\000\005\000\006\000\000\000\
\008\000\027\000\028\000\029\000\030\000\031\000\032\000\000\000\
\000\000\000\000\009\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\033\000\000\000\035\000\012\000\009\000\
\009\000\009\000\000\000\000\000\009\000\009\000\009\000\009\000\
\009\000\009\000\000\000\000\000\000\000\009\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\009\000\000\000\
\009\000\009\000\004\000\005\000\006\000\000\000\076\000\027\000\
\028\000\029\000\030\000\031\000\032\000\000\000\000\000\000\000\
\009\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\085\000\000\000\035\000\012\000\004\000\005\000\006\000\
\000\000\000\000\027\000\028\000\029\000\030\000\031\000\032\000\
\000\000\000\000\000\000\009\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\039\000\000\000\035\000\012\000\
\004\000\005\000\006\000\000\000\000\000\027\000\028\000\029\000\
\030\000\031\000\032\000\000\000\043\000\000\000\009\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\004\000\005\000\
\006\000\035\000\012\000\027\000\028\000\029\000\030\000\031\000\
\032\000\000\000\000\000\000\000\009\000\044\000\000\000\000\000\
\000\000\000\000\000\000\000\000\004\000\005\000\006\000\035\000\
\012\000\027\000\028\000\029\000\030\000\031\000\032\000\000\000\
\000\000\000\000\009\000\000\000\004\000\005\000\006\000\046\000\
\000\000\000\000\000\000\029\000\030\000\035\000\012\000\004\000\
\005\000\006\000\009\000\076\000\027\000\028\000\029\000\030\000\
\031\000\032\000\000\000\000\000\000\000\009\000\012\000\000\000\
\000\000\000\000\000\000\000\000\000\000\004\000\005\000\006\000\
\035\000\012\000\027\000\028\000\029\000\030\000\031\000\032\000\
\000\000\000\000\077\000\009\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\004\000\005\000\006\000\035\000\012\000\
\027\000\028\000\029\000\030\000\031\000\032\000\000\000\000\000\
\000\000\009\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\088\000\000\000\035\000\012\000\004\000\005\000\
\006\000\000\000\000\000\027\000\028\000\029\000\030\000\031\000\
\032\000\000\000\000\000\000\000\009\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\095\000\000\000\035\000\
\012\000\004\000\005\000\006\000\000\000\097\000\027\000\028\000\
\029\000\030\000\031\000\032\000\000\000\000\000\000\000\009\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\004\000\
\005\000\006\000\035\000\012\000\027\000\028\000\029\000\030\000\
\031\000\032\000\000\000\000\000\000\000\009\000\000\000\000\000\
\000\000\000\000\000\000\100\000\000\000\004\000\005\000\006\000\
\035\000\012\000\027\000\028\000\029\000\030\000\031\000\032\000\
\000\000\000\000\000\000\009\000\000\000\004\000\005\000\006\000\
\016\000\000\000\000\000\000\000\000\000\000\000\035\000\012\000\
\008\000\000\000\000\000\009\000\000\000\010\000\000\000\011\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\012\000"

let yycheck = "\037\000\
\000\000\007\000\003\001\003\001\010\000\066\000\016\001\003\001\
\018\001\028\001\016\000\002\000\003\001\010\001\020\000\025\001\
\018\001\008\000\009\000\010\001\011\000\082\000\023\001\023\001\
\026\001\000\000\003\001\001\000\002\000\018\001\091\000\037\000\
\010\001\010\001\005\001\005\001\027\000\028\000\029\000\030\000\
\031\000\032\000\017\001\104\000\035\000\001\001\002\001\003\001\
\004\001\010\001\000\000\042\000\043\000\022\001\045\000\028\001\
\012\001\025\001\026\001\015\001\017\001\017\001\026\001\019\001\
\018\001\056\000\018\001\073\000\059\000\016\001\108\000\027\001\
\025\001\026\001\010\001\000\000\022\001\083\000\016\001\005\001\
\071\000\100\000\104\000\074\000\255\255\076\000\077\000\255\255\
\255\255\255\255\081\000\255\255\014\000\255\255\255\255\000\000\
\018\000\001\001\002\001\003\001\022\000\023\000\108\000\025\000\
\255\255\096\000\097\000\001\001\002\001\003\001\101\000\015\001\
\255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
\255\255\015\001\255\255\027\001\255\255\047\000\048\000\049\000\
\050\000\051\000\052\000\053\000\255\255\027\001\255\255\000\000\
\255\255\255\255\060\000\061\000\062\000\255\255\255\255\255\255\
\255\255\255\255\255\255\000\000\255\255\255\255\072\000\255\255\
\255\255\075\000\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\000\000\084\000\255\255\255\255\087\000\255\255\089\000\
\090\000\255\255\092\000\255\255\255\255\000\000\255\255\255\255\
\255\255\255\255\255\255\255\255\102\000\103\000\255\255\255\255\
\255\255\107\000\255\255\000\000\255\255\001\001\002\001\003\001\
\255\255\255\255\006\001\007\001\008\001\009\001\010\001\011\001\
\255\255\255\255\255\255\015\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\000\000\024\001\255\255\026\001\027\001\
\255\255\029\001\001\001\002\001\003\001\255\255\255\255\006\001\
\007\001\008\001\009\001\010\001\011\001\255\255\255\255\255\255\
\015\001\255\255\001\001\002\001\003\001\255\255\000\000\006\001\
\007\001\008\001\009\001\026\001\027\001\255\255\029\001\255\255\
\015\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\026\001\027\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\255\255\013\001\014\001\255\255\
\016\001\255\255\255\255\255\255\020\001\021\001\022\001\255\255\
\024\001\255\255\026\001\255\255\255\255\029\001\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\255\255\013\001\014\001\
\255\255\016\001\255\255\255\255\255\255\020\001\021\001\022\001\
\255\255\024\001\255\255\026\001\255\255\255\255\029\001\005\001\
\006\001\007\001\255\255\255\255\010\001\011\001\255\255\013\001\
\014\001\255\255\016\001\255\255\255\255\255\255\020\001\021\001\
\022\001\255\255\024\001\255\255\026\001\255\255\255\255\029\001\
\005\001\006\001\007\001\255\255\255\255\010\001\011\001\255\255\
\013\001\014\001\255\255\016\001\255\255\255\255\255\255\020\001\
\021\001\022\001\255\255\024\001\005\001\026\001\255\255\255\255\
\029\001\010\001\011\001\255\255\013\001\014\001\255\255\016\001\
\255\255\255\255\255\255\020\001\021\001\022\001\255\255\024\001\
\005\001\255\255\255\255\255\255\029\001\010\001\011\001\255\255\
\013\001\014\001\255\255\016\001\255\255\255\255\255\255\020\001\
\021\001\022\001\255\255\024\001\005\001\255\255\255\255\255\255\
\029\001\010\001\011\001\255\255\013\001\014\001\255\255\016\001\
\005\001\255\255\255\255\020\001\021\001\022\001\255\255\024\001\
\013\001\014\001\255\255\016\001\029\001\255\255\005\001\020\001\
\021\001\022\001\255\255\024\001\255\255\255\255\013\001\014\001\
\029\001\016\001\005\001\255\255\255\255\020\001\021\001\022\001\
\255\255\024\001\013\001\014\001\255\255\016\001\029\001\255\255\
\005\001\020\001\021\001\022\001\255\255\024\001\255\255\255\255\
\013\001\014\001\029\001\016\001\255\255\255\255\255\255\020\001\
\021\001\022\001\255\255\024\001\001\001\002\001\003\001\255\255\
\029\001\006\001\007\001\008\001\009\001\010\001\011\001\255\255\
\255\255\255\255\015\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\024\001\255\255\026\001\027\001\001\001\
\002\001\003\001\255\255\255\255\006\001\007\001\008\001\009\001\
\010\001\011\001\255\255\255\255\255\255\015\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\024\001\255\255\
\026\001\027\001\001\001\002\001\003\001\255\255\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\255\255\255\255\255\255\
\015\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\024\001\255\255\026\001\027\001\001\001\002\001\003\001\
\255\255\255\255\006\001\007\001\008\001\009\001\010\001\011\001\
\255\255\255\255\255\255\015\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\024\001\255\255\026\001\027\001\
\001\001\002\001\003\001\255\255\255\255\006\001\007\001\008\001\
\009\001\010\001\011\001\255\255\013\001\255\255\015\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\001\001\002\001\
\003\001\026\001\027\001\006\001\007\001\008\001\009\001\010\001\
\011\001\255\255\255\255\255\255\015\001\016\001\255\255\255\255\
\255\255\255\255\255\255\255\255\001\001\002\001\003\001\026\001\
\027\001\006\001\007\001\008\001\009\001\010\001\011\001\255\255\
\255\255\255\255\015\001\255\255\001\001\002\001\003\001\020\001\
\255\255\255\255\255\255\008\001\009\001\026\001\027\001\001\001\
\002\001\003\001\015\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\255\255\255\255\255\255\015\001\027\001\255\255\
\255\255\255\255\255\255\255\255\255\255\001\001\002\001\003\001\
\026\001\027\001\006\001\007\001\008\001\009\001\010\001\011\001\
\255\255\255\255\014\001\015\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\001\001\002\001\003\001\026\001\027\001\
\006\001\007\001\008\001\009\001\010\001\011\001\255\255\255\255\
\255\255\015\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\024\001\255\255\026\001\027\001\001\001\002\001\
\003\001\255\255\255\255\006\001\007\001\008\001\009\001\010\001\
\011\001\255\255\255\255\255\255\015\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\024\001\255\255\026\001\
\027\001\001\001\002\001\003\001\255\255\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\255\255\255\255\255\255\015\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\001\001\
\002\001\003\001\026\001\027\001\006\001\007\001\008\001\009\001\
\010\001\011\001\255\255\255\255\255\255\015\001\255\255\255\255\
\255\255\255\255\255\255\021\001\255\255\001\001\002\001\003\001\
\026\001\027\001\006\001\007\001\008\001\009\001\010\001\011\001\
\255\255\255\255\255\255\015\001\255\255\001\001\002\001\003\001\
\004\001\255\255\255\255\255\255\255\255\255\255\026\001\027\001\
\012\001\255\255\255\255\015\001\255\255\017\001\255\255\019\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\027\001"

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
# 45 "ex1Parser.mly"
           ( _1 )
# 403 "ex1Parser.ml"
               : Ex1Syntax.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 46 "ex1Parser.mly"
             ( _1 )
# 410 "ex1Parser.ml"
               : Ex1Syntax.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 50 "ex1Parser.mly"
              ( CExp _1 )
# 417 "ex1Parser.ml"
               : Ex1Syntax.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 51 "ex1Parser.mly"
                         ( CLet (_2, _4) )
# 425 "ex1Parser.ml"
               : Ex1Syntax.command))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'var) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 52 "ex1Parser.mly"
                                           ( CRecFun (_3,_6,_8) )
# 434 "ex1Parser.ml"
               : Ex1Syntax.command))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'fun_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 53 "ex1Parser.mly"
                                  ( CRecFunand _3 )
# 442 "ex1Parser.ml"
               : Ex1Syntax.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 58 "ex1Parser.mly"
                                 ( ELet(_2,_4,_6) )
# 451 "ex1Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 59 "ex1Parser.mly"
                                       ( ERecFun (_3,_4,_6,_8) )
# 461 "ex1Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'fun_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 60 "ex1Parser.mly"
                                  ( ERecFunand (_3, _5) )
# 469 "ex1Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 61 "ex1Parser.mly"
                                 ( EIf(_2,_4,_6) )
# 478 "ex1Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 62 "ex1Parser.mly"
                 ( EEqual(_1, _3) )
# 486 "ex1Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 63 "ex1Parser.mly"
                 ( ECompare(_1, _3) )
# 494 "ex1Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 64 "ex1Parser.mly"
                    ( EBin(OpAdd, _1, _3) )
# 502 "ex1Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 65 "ex1Parser.mly"
                    ( EBin(OpSub, _1, _3) )
# 510 "ex1Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 66 "ex1Parser.mly"
                    ( EBin(OpMul, _1, _3) )
# 518 "ex1Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 67 "ex1Parser.mly"
                     ( EBin(OpDiv, _1, _3) )
# 526 "ex1Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 68 "ex1Parser.mly"
                       ( EFun(_2,_4) )
# 534 "ex1Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'apply_expr) in
    Obj.repr(
# 69 "ex1Parser.mly"
                       (EApp (_1,_2))
# 542 "ex1Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'pattern_expr) in
    Obj.repr(
# 70 "ex1Parser.mly"
                                     ( EMatch (_2, _4) )
# 550 "ex1Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 71 "ex1Parser.mly"
                   ( ECons (_1, _3) )
# 558 "ex1Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'apply_expr) in
    Obj.repr(
# 72 "ex1Parser.mly"
               ( _1 )
# 565 "ex1Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 76 "ex1Parser.mly"
                              ( [(_1, _4, _6)] )
# 574 "ex1Parser.ml"
               : 'fun_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'fun_list) in
    Obj.repr(
# 77 "ex1Parser.mly"
                                           ( (_1, _4, _6) :: _8 )
# 584 "ex1Parser.ml"
               : 'fun_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 82 "ex1Parser.mly"
         ( [_1] )
# 591 "ex1Parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_list) in
    Obj.repr(
# 83 "ex1Parser.mly"
                         ( _1 :: _3 )
# 599 "ex1Parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 88 "ex1Parser.mly"
                        ( [(_1, _3)] )
# 607 "ex1Parser.ml"
               : 'pattern_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'pattern_expr) in
    Obj.repr(
# 89 "ex1Parser.mly"
                                       ( (_1, _3) :: _5 )
# 616 "ex1Parser.ml"
               : 'pattern_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 93 "ex1Parser.mly"
            ( PInt _1 )
# 623 "ex1Parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 94 "ex1Parser.mly"
            ( PBool _1 )
# 630 "ex1Parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 95 "ex1Parser.mly"
            ( PVar _1 )
# 637 "ex1Parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "ex1Parser.mly"
                      ( PNil )
# 643 "ex1Parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'pattern) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'pattern_list) in
    Obj.repr(
# 97 "ex1Parser.mly"
                                         ( PTuple (_2 :: _4) )
# 651 "ex1Parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pattern) in
    Obj.repr(
# 98 "ex1Parser.mly"
                         ( PCons (_1, _3) )
# 659 "ex1Parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'pattern) in
    Obj.repr(
# 102 "ex1Parser.mly"
            ( [_1] )
# 666 "ex1Parser.ml"
               : 'pattern_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pattern_list) in
    Obj.repr(
# 103 "ex1Parser.mly"
                               ( _1 :: _3 )
# 674 "ex1Parser.ml"
               : 'pattern_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 107 "ex1Parser.mly"
                    ( EValue (VInt _1) )
# 681 "ex1Parser.ml"
               : 'apply_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 108 "ex1Parser.mly"
                    ( EValue (VBool _1) )
# 688 "ex1Parser.ml"
               : 'apply_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 109 "ex1Parser.mly"
                    ( EVar _1 )
# 695 "ex1Parser.ml"
               : 'apply_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 110 "ex1Parser.mly"
                    ( _2 )
# 702 "ex1Parser.ml"
               : 'apply_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 111 "ex1Parser.mly"
                       ( ENil )
# 708 "ex1Parser.ml"
               : 'apply_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 117 "ex1Parser.mly"
       ( _1 )
# 715 "ex1Parser.ml"
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ex1Syntax.expr)
let command (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : Ex1Syntax.command)
