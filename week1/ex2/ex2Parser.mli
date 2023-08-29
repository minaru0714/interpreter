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

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ex2Syntax.expr
