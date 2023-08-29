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
  | SHARP
  | END
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ex4Syntax.expr
val command :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ex4Syntax.command
