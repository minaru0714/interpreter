type token =
  | INT of (int)
  | BOOL of (bool)
  | ID of (string)
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
  | SHARP
  | END
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ex3Syntax.expr
