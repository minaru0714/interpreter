type token =
  | INT of (int)
  | BOOL of (bool)
  | ID of (string)
  | LET
  | IN
  | EQ
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | LT
  | IF
  | THEN
  | ELSE
  | LPAR
  | RPAR
  | FUN
  | ARROW
  | SHARP
  | END
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Week2ex1Syntax.expr
val command :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Week2ex1Syntax.command
