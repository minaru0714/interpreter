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
  | UNIT

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> LazySyntax.expr
val command :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> LazySyntax.command
