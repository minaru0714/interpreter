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

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ex4Syntax.expr
val command :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ex4Syntax.command
