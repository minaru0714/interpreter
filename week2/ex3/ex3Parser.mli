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

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ex3Syntax.expr
val command :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ex3Syntax.command
