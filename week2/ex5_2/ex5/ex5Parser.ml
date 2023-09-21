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
# 2 "ex5Parser.mly"
  open Ex5Syntax
# 38 "ex5Parser.ml"
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
\003\000\003\000\003\000\003\000\005\000\005\000\006\000\006\000\
\009\000\009\000\008\000\008\000\010\000\010\000\010\000\010\000\
\010\000\010\000\011\000\011\000\007\000\007\000\007\000\007\000\
\007\000\004\000\000\000\000\000"

let yylen = "\002\000\
\002\000\002\000\005\000\006\000\008\000\006\000\007\000\008\000\
\006\000\003\000\003\000\003\000\003\000\003\000\003\000\004\000\
\002\000\005\000\003\000\001\000\002\000\002\000\006\000\000\000\
\001\000\003\000\003\000\005\000\001\000\001\000\001\000\002\000\
\005\000\003\000\001\000\003\000\001\000\001\000\001\000\003\000\
\002\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\037\000\038\000\039\000\000\000\000\000\
\000\000\000\000\000\000\000\000\043\000\000\000\020\000\000\000\
\044\000\000\000\042\000\000\000\000\000\000\000\000\000\000\000\
\000\000\041\000\000\000\000\000\000\000\000\000\000\000\000\000\
\001\000\000\000\017\000\000\000\000\000\002\000\000\000\000\000\
\000\000\040\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\029\000\030\000\031\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\032\000\018\000\000\000\000\000\000\000\000\000\003\000\000\000\
\022\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\004\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\033\000\028\000\005\000\
\000\000\036\000\000\000\023\000"

let yydgoto = "\003\000\
\013\000\017\000\014\000\068\000\069\000\084\000\015\000\063\000\
\000\000\064\000\096\000"

let yysindex = "\017\000\
\136\001\155\001\000\000\000\000\000\000\000\000\253\254\136\001\
\136\001\005\255\136\001\238\254\000\000\224\000\000\000\006\255\
\000\000\157\255\000\000\005\255\031\255\230\255\246\000\045\255\
\012\001\000\000\136\001\136\001\136\001\136\001\136\001\136\001\
\000\000\136\001\000\000\005\255\057\255\000\000\005\255\136\001\
\136\001\000\000\136\001\021\255\190\255\190\255\037\255\037\255\
\120\001\120\001\120\001\005\255\136\001\052\255\039\001\061\001\
\105\001\000\000\000\000\000\000\021\255\041\255\049\255\245\254\
\072\255\201\255\136\001\052\255\254\254\136\001\136\001\024\255\
\000\000\000\000\136\001\021\255\136\001\048\255\000\000\105\001\
\000\000\136\001\005\255\076\255\105\001\105\001\021\255\083\001\
\060\255\105\001\000\000\105\001\005\255\136\001\033\255\067\255\
\021\255\063\255\085\255\105\001\021\255\000\000\000\000\000\000\
\136\001\000\000\111\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\051\000\076\000\001\000\026\000\
\096\000\116\000\136\000\000\000\000\000\000\000\000\000\000\000\
\148\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\055\255\
\000\000\000\000\000\000\000\000\162\000\174\000\000\000\079\255\
\029\255\055\255\000\000\188\000\000\000\000\000\086\255\000\000\
\000\000\000\000\000\000\200\000\000\000\000\000\000\000\000\000\
\000\000\000\000\001\255\000\000"

let yygindex = "\000\000\
\000\000\000\000\003\000\137\000\204\255\170\255\043\000\006\000\
\000\000\197\255\004\000"

let yytablesize = 694
let yytable = "\019\000\
\014\000\072\000\082\000\098\000\018\000\024\000\075\000\019\000\
\019\000\026\000\022\000\023\000\078\000\025\000\076\000\081\000\
\089\000\001\000\002\000\020\000\108\000\058\000\059\000\060\000\
\024\000\015\000\083\000\095\000\036\000\045\000\046\000\047\000\
\048\000\049\000\050\000\061\000\051\000\004\000\005\000\006\000\
\040\000\095\000\055\000\056\000\034\000\057\000\034\000\062\000\
\087\000\076\000\012\000\009\000\082\000\034\000\019\000\066\000\
\035\000\101\000\076\000\021\000\035\000\067\000\043\000\012\000\
\035\000\035\000\053\000\035\000\073\000\080\000\074\000\091\000\
\085\000\086\000\019\000\013\000\083\000\088\000\021\000\090\000\
\094\000\077\000\102\000\021\000\092\000\076\000\104\000\035\000\
\035\000\035\000\035\000\035\000\035\000\035\000\105\000\010\000\
\100\000\035\000\035\000\035\000\027\000\035\000\103\000\000\000\
\106\000\000\000\000\000\107\000\035\000\000\000\000\000\004\000\
\005\000\006\000\000\000\011\000\027\000\028\000\029\000\030\000\
\031\000\032\000\035\000\000\000\000\000\009\000\000\000\035\000\
\035\000\000\000\035\000\000\000\035\000\000\000\035\000\019\000\
\034\000\012\000\000\000\083\000\000\000\000\000\035\000\021\000\
\000\000\000\000\024\000\016\000\000\000\035\000\000\000\000\000\
\037\000\000\000\000\000\000\000\039\000\004\000\005\000\006\000\
\000\000\006\000\027\000\028\000\029\000\030\000\031\000\032\000\
\000\000\000\000\000\000\009\000\052\000\009\000\000\000\054\000\
\000\000\000\000\000\000\000\000\038\000\000\000\034\000\012\000\
\000\000\000\000\000\000\007\000\065\000\000\000\004\000\005\000\
\006\000\000\000\000\000\000\000\000\000\029\000\030\000\008\000\
\000\000\004\000\005\000\006\000\009\000\070\000\027\000\028\000\
\029\000\030\000\031\000\032\000\000\000\000\000\000\000\009\000\
\012\000\000\000\000\000\093\000\000\000\000\000\000\000\033\000\
\079\000\000\000\034\000\012\000\000\000\099\000\004\000\005\000\
\006\000\000\000\000\000\027\000\028\000\029\000\030\000\031\000\
\032\000\000\000\041\000\000\000\009\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\034\000\
\012\000\000\000\000\000\000\000\000\000\014\000\014\000\014\000\
\014\000\014\000\014\000\014\000\000\000\014\000\014\000\000\000\
\014\000\000\000\000\000\000\000\014\000\014\000\014\000\000\000\
\014\000\000\000\014\000\000\000\000\000\014\000\015\000\015\000\
\015\000\015\000\015\000\015\000\015\000\000\000\015\000\015\000\
\000\000\015\000\000\000\000\000\000\000\015\000\015\000\015\000\
\000\000\015\000\000\000\015\000\000\000\000\000\015\000\012\000\
\012\000\012\000\000\000\000\000\012\000\012\000\000\000\012\000\
\012\000\000\000\012\000\000\000\000\000\000\000\012\000\012\000\
\012\000\000\000\012\000\000\000\012\000\000\000\000\000\012\000\
\013\000\013\000\013\000\000\000\000\000\013\000\013\000\000\000\
\013\000\013\000\000\000\013\000\000\000\000\000\000\000\013\000\
\013\000\013\000\000\000\013\000\010\000\013\000\000\000\000\000\
\013\000\010\000\010\000\000\000\010\000\010\000\000\000\010\000\
\000\000\000\000\000\000\010\000\010\000\010\000\000\000\010\000\
\011\000\000\000\000\000\000\000\010\000\011\000\011\000\000\000\
\011\000\011\000\000\000\011\000\000\000\000\000\000\000\011\000\
\011\000\011\000\000\000\011\000\019\000\000\000\000\000\000\000\
\011\000\019\000\019\000\000\000\019\000\019\000\000\000\019\000\
\016\000\000\000\000\000\019\000\019\000\019\000\000\000\019\000\
\016\000\016\000\000\000\016\000\019\000\000\000\006\000\016\000\
\016\000\016\000\000\000\016\000\000\000\000\000\006\000\006\000\
\016\000\006\000\009\000\000\000\000\000\006\000\006\000\006\000\
\000\000\006\000\009\000\009\000\000\000\009\000\006\000\000\000\
\007\000\009\000\009\000\009\000\000\000\009\000\000\000\000\000\
\007\000\007\000\009\000\007\000\008\000\000\000\000\000\007\000\
\007\000\007\000\000\000\007\000\008\000\008\000\000\000\008\000\
\007\000\000\000\000\000\008\000\008\000\008\000\000\000\008\000\
\004\000\005\000\006\000\000\000\008\000\027\000\028\000\029\000\
\030\000\031\000\032\000\000\000\000\000\000\000\009\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\004\000\005\000\
\006\000\034\000\012\000\027\000\028\000\029\000\030\000\031\000\
\032\000\000\000\000\000\000\000\009\000\042\000\000\000\000\000\
\000\000\000\000\000\000\000\000\004\000\005\000\006\000\034\000\
\012\000\027\000\028\000\029\000\030\000\031\000\032\000\000\000\
\000\000\000\000\009\000\000\000\000\000\000\000\000\000\044\000\
\000\000\000\000\000\000\000\000\000\000\034\000\012\000\004\000\
\005\000\006\000\000\000\070\000\027\000\028\000\029\000\030\000\
\031\000\032\000\000\000\000\000\000\000\009\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\004\000\005\000\006\000\
\034\000\012\000\027\000\028\000\029\000\030\000\031\000\032\000\
\000\000\000\000\071\000\009\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\004\000\005\000\006\000\034\000\012\000\
\027\000\028\000\029\000\030\000\031\000\032\000\000\000\000\000\
\000\000\009\000\000\000\000\000\000\000\000\000\000\000\097\000\
\000\000\004\000\005\000\006\000\034\000\012\000\027\000\028\000\
\029\000\030\000\031\000\032\000\000\000\000\000\000\000\009\000\
\004\000\005\000\006\000\000\000\000\000\027\000\028\000\029\000\
\030\000\000\000\034\000\012\000\000\000\000\000\009\000\000\000\
\004\000\005\000\006\000\007\000\000\000\000\000\000\000\000\000\
\000\000\034\000\012\000\008\000\000\000\000\000\009\000\000\000\
\010\000\000\000\011\000\004\000\005\000\006\000\016\000\000\000\
\000\000\000\000\012\000\000\000\000\000\000\000\008\000\000\000\
\000\000\009\000\000\000\010\000\000\000\011\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\012\000"

let yycheck = "\003\001\
\000\000\061\000\005\001\090\000\002\000\005\001\018\001\003\001\
\003\001\028\001\008\000\009\000\065\000\011\000\026\001\068\000\
\076\000\001\000\002\000\023\001\107\000\001\001\002\001\003\001\
\024\001\000\000\029\001\087\000\023\001\027\000\028\000\029\000\
\030\000\031\000\032\000\015\001\034\000\001\001\002\001\003\001\
\010\001\101\000\040\000\041\000\016\001\043\000\018\001\027\001\
\025\001\026\001\000\000\015\001\005\001\025\001\003\001\053\000\
\014\000\025\001\026\001\005\001\018\000\010\001\018\001\027\001\
\022\000\023\000\010\001\025\000\028\001\067\000\022\001\024\001\
\070\000\071\000\003\001\000\000\029\001\075\000\024\001\077\000\
\005\001\010\001\016\001\029\001\082\000\026\001\024\001\045\000\
\046\000\047\000\048\000\049\000\050\000\051\000\010\001\000\000\
\094\000\055\000\056\000\057\000\022\001\016\001\097\000\255\255\
\101\000\255\255\255\255\105\000\066\000\255\255\255\255\001\001\
\002\001\003\001\255\255\000\000\006\001\007\001\008\001\009\001\
\010\001\011\001\080\000\255\255\255\255\015\001\255\255\085\000\
\086\000\255\255\088\000\255\255\090\000\255\255\092\000\000\000\
\026\001\027\001\255\255\029\001\255\255\255\255\100\000\007\000\
\255\255\255\255\010\000\000\000\255\255\107\000\255\255\255\255\
\016\000\255\255\255\255\255\255\020\000\001\001\002\001\003\001\
\255\255\000\000\006\001\007\001\008\001\009\001\010\001\011\001\
\255\255\255\255\255\255\015\001\036\000\000\000\255\255\039\000\
\255\255\255\255\255\255\255\255\024\001\255\255\026\001\027\001\
\255\255\255\255\255\255\000\000\052\000\255\255\001\001\002\001\
\003\001\255\255\255\255\255\255\255\255\008\001\009\001\000\000\
\255\255\001\001\002\001\003\001\015\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\255\255\255\255\255\255\015\001\
\027\001\255\255\255\255\083\000\255\255\255\255\255\255\000\000\
\024\001\255\255\026\001\027\001\255\255\093\000\001\001\002\001\
\003\001\255\255\255\255\006\001\007\001\008\001\009\001\010\001\
\011\001\255\255\013\001\255\255\015\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\026\001\
\027\001\255\255\255\255\255\255\255\255\005\001\006\001\007\001\
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
\013\001\014\001\029\001\016\001\005\001\255\255\255\255\020\001\
\021\001\022\001\255\255\024\001\013\001\014\001\255\255\016\001\
\029\001\255\255\255\255\020\001\021\001\022\001\255\255\024\001\
\001\001\002\001\003\001\255\255\029\001\006\001\007\001\008\001\
\009\001\010\001\011\001\255\255\255\255\255\255\015\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\001\001\002\001\
\003\001\026\001\027\001\006\001\007\001\008\001\009\001\010\001\
\011\001\255\255\255\255\255\255\015\001\016\001\255\255\255\255\
\255\255\255\255\255\255\255\255\001\001\002\001\003\001\026\001\
\027\001\006\001\007\001\008\001\009\001\010\001\011\001\255\255\
\255\255\255\255\015\001\255\255\255\255\255\255\255\255\020\001\
\255\255\255\255\255\255\255\255\255\255\026\001\027\001\001\001\
\002\001\003\001\255\255\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\255\255\255\255\255\255\015\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\001\001\002\001\003\001\
\026\001\027\001\006\001\007\001\008\001\009\001\010\001\011\001\
\255\255\255\255\014\001\015\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\001\001\002\001\003\001\026\001\027\001\
\006\001\007\001\008\001\009\001\010\001\011\001\255\255\255\255\
\255\255\015\001\255\255\255\255\255\255\255\255\255\255\021\001\
\255\255\001\001\002\001\003\001\026\001\027\001\006\001\007\001\
\008\001\009\001\010\001\011\001\255\255\255\255\255\255\015\001\
\001\001\002\001\003\001\255\255\255\255\006\001\007\001\008\001\
\009\001\255\255\026\001\027\001\255\255\255\255\015\001\255\255\
\001\001\002\001\003\001\004\001\255\255\255\255\255\255\255\255\
\255\255\026\001\027\001\012\001\255\255\255\255\015\001\255\255\
\017\001\255\255\019\001\001\001\002\001\003\001\004\001\255\255\
\255\255\255\255\027\001\255\255\255\255\255\255\012\001\255\255\
\255\255\015\001\255\255\017\001\255\255\019\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\027\001"

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
# 45 "ex5Parser.mly"
           ( _1 )
# 369 "ex5Parser.ml"
               : Ex5Syntax.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 49 "ex5Parser.mly"
              ( CExp _1 )
# 376 "ex5Parser.ml"
               : Ex5Syntax.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 50 "ex5Parser.mly"
                         ( CLet (_2, _4) )
# 384 "ex5Parser.ml"
               : Ex5Syntax.command))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'var_expr) in
    Obj.repr(
# 51 "ex5Parser.mly"
                                  ( CRecFun (_3, _4, _5) )
# 393 "ex5Parser.ml"
               : Ex5Syntax.command))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'and_expr) in
    Obj.repr(
# 52 "ex5Parser.mly"
                                           ( CRecFunand  ((_3, _4, _6) :: _7) )
# 403 "ex5Parser.ml"
               : Ex5Syntax.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 56 "ex5Parser.mly"
                                 ( ELet(_2,_4,_6) )
# 412 "ex5Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'var_expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 57 "ex5Parser.mly"
                                      ( ERecFun (_3, _4, _5, _7) )
# 422 "ex5Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'var_expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'and_expr) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 58 "ex5Parser.mly"
                                               ( ERecFunand (((_3,_4,_5) :: _6), _8) )
# 433 "ex5Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 59 "ex5Parser.mly"
                                 ( EIf(_2,_4,_6) )
# 442 "ex5Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 60 "ex5Parser.mly"
                 ( EEqual(_1, _3) )
# 450 "ex5Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 61 "ex5Parser.mly"
                 ( ECompare(_1, _3) )
# 458 "ex5Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 62 "ex5Parser.mly"
                    ( EBin(OpAdd, _1, _3) )
# 466 "ex5Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 63 "ex5Parser.mly"
                    ( EBin(OpSub, _1, _3) )
# 474 "ex5Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 64 "ex5Parser.mly"
                    ( EBin(OpMul, _1, _3) )
# 482 "ex5Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 65 "ex5Parser.mly"
                     ( EBin(OpDiv, _1, _3) )
# 490 "ex5Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 66 "ex5Parser.mly"
                       ( EFun(_2,_4) )
# 498 "ex5Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'apply_expr) in
    Obj.repr(
# 67 "ex5Parser.mly"
                       (EApp (_1,_2))
# 506 "ex5Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'pattern_expr) in
    Obj.repr(
# 68 "ex5Parser.mly"
                                     ( EMatch (_2, _4) )
# 514 "ex5Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 69 "ex5Parser.mly"
                   ( ECons (_1, _3) )
# 522 "ex5Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'apply_expr) in
    Obj.repr(
# 70 "ex5Parser.mly"
               ( _1 )
# 529 "ex5Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 75 "ex5Parser.mly"
            ( _2 )
# 536 "ex5Parser.ml"
               : 'var_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'var) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'var_expr) in
    Obj.repr(
# 76 "ex5Parser.mly"
                 ( EFun (_1, _2) )
# 544 "ex5Parser.ml"
               : 'var_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'and_expr) in
    Obj.repr(
# 81 "ex5Parser.mly"
                                  ( (_2,_3,_5) :: _6 )
# 554 "ex5Parser.ml"
               : 'and_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "ex5Parser.mly"
                            ( [] )
# 560 "ex5Parser.ml"
               : 'and_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 86 "ex5Parser.mly"
         ( [_1] )
# 567 "ex5Parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_list) in
    Obj.repr(
# 87 "ex5Parser.mly"
                         ( _1 :: _3 )
# 575 "ex5Parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 92 "ex5Parser.mly"
                        ( [(_1, _3)] )
# 583 "ex5Parser.ml"
               : 'pattern_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'pattern_expr) in
    Obj.repr(
# 93 "ex5Parser.mly"
                                       ( (_1, _3) :: _5 )
# 592 "ex5Parser.ml"
               : 'pattern_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 97 "ex5Parser.mly"
            ( PInt _1 )
# 599 "ex5Parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 98 "ex5Parser.mly"
            ( PBool _1 )
# 606 "ex5Parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 99 "ex5Parser.mly"
            ( PVar _1 )
# 613 "ex5Parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    Obj.repr(
# 100 "ex5Parser.mly"
                      ( PNil )
# 619 "ex5Parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'pattern) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'pattern_list) in
    Obj.repr(
# 101 "ex5Parser.mly"
                                         ( PTuple (_2 :: _4) )
# 627 "ex5Parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pattern) in
    Obj.repr(
# 102 "ex5Parser.mly"
                         ( PCons (_1, _3) )
# 635 "ex5Parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'pattern) in
    Obj.repr(
# 106 "ex5Parser.mly"
            ( [_1] )
# 642 "ex5Parser.ml"
               : 'pattern_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pattern_list) in
    Obj.repr(
# 107 "ex5Parser.mly"
                               ( _1 :: _3 )
# 650 "ex5Parser.ml"
               : 'pattern_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 111 "ex5Parser.mly"
                    ( EValue (VInt _1) )
# 657 "ex5Parser.ml"
               : 'apply_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 112 "ex5Parser.mly"
                    ( EValue (VBool _1) )
# 664 "ex5Parser.ml"
               : 'apply_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 113 "ex5Parser.mly"
                    ( EVar _1 )
# 671 "ex5Parser.ml"
               : 'apply_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 114 "ex5Parser.mly"
                    ( _2 )
# 678 "ex5Parser.ml"
               : 'apply_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 115 "ex5Parser.mly"
                       ( ENil )
# 684 "ex5Parser.ml"
               : 'apply_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 121 "ex5Parser.mly"
       ( _1 )
# 691 "ex5Parser.ml"
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ex5Syntax.expr)
let command (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : Ex5Syntax.command)