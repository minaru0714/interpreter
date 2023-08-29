# 1 "ex2Lexer.mll"
 
    open Ex2Parser

# 6 "ex2Lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\240\255\241\255\000\000\014\000\000\000\000\000\001\000\
    \247\255\248\255\249\255\250\255\251\255\252\255\253\255\254\255\
    \002\000\246\255\000\000\005\000\000\000\245\255\006\000\242\255\
    \000\000\008\000\244\255\003\000\003\000\011\000";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\015\000\012\000\015\000\015\000\015\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_default =
   "\001\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \255\255\000\000\255\255\255\255\255\255\000\000\255\255\000\000\
    \255\255\255\255\000\000\255\255\255\255\255\255";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\016\000\016\000\016\000\016\000\016\000\000\000\016\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \016\000\000\000\016\000\000\000\000\000\000\000\000\000\000\000\
    \009\000\008\000\011\000\013\000\000\000\012\000\000\000\010\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\000\000\000\000\014\000\015\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\027\000\000\000\000\000\000\000\005\000\003\000\017\000\
    \019\000\007\000\020\000\023\000\024\000\026\000\021\000\028\000\
    \023\000\000\000\018\000\025\000\006\000\022\000\029\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\016\000\016\000\000\000\255\255\016\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\016\000\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\000\000\000\000\255\255\000\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\255\255\000\000\000\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\003\000\255\255\255\255\255\255\000\000\000\000\007\000\
    \006\000\000\000\019\000\022\000\005\000\025\000\020\000\027\000\
    \029\000\255\255\006\000\024\000\000\000\018\000\028\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec token lexbuf =
   __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 10 "ex2Lexer.mll"
              ( token lexbuf )
# 114 "ex2Lexer.ml"

  | 1 ->
# 11 "ex2Lexer.mll"
              ( EQ )
# 119 "ex2Lexer.ml"

  | 2 ->
# 12 "ex2Lexer.mll"
              ( LT )
# 124 "ex2Lexer.ml"

  | 3 ->
# 13 "ex2Lexer.mll"
              ( PLUS )
# 129 "ex2Lexer.ml"

  | 4 ->
# 14 "ex2Lexer.mll"
              ( MINUS )
# 134 "ex2Lexer.ml"

  | 5 ->
# 15 "ex2Lexer.mll"
              ( TIMES )
# 139 "ex2Lexer.ml"

  | 6 ->
# 16 "ex2Lexer.mll"
              ( DIV )
# 144 "ex2Lexer.ml"

  | 7 ->
# 17 "ex2Lexer.mll"
              ( LPAR )
# 149 "ex2Lexer.ml"

  | 8 ->
# 18 "ex2Lexer.mll"
              ( RPAR )
# 154 "ex2Lexer.ml"

  | 9 ->
# 19 "ex2Lexer.mll"
              ( IF )
# 159 "ex2Lexer.ml"

  | 10 ->
# 20 "ex2Lexer.mll"
              ( THEN )
# 164 "ex2Lexer.ml"

  | 11 ->
# 21 "ex2Lexer.mll"
              ( ELSE )
# 169 "ex2Lexer.ml"

  | 12 ->
let
# 22 "ex2Lexer.mll"
            n
# 175 "ex2Lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 22 "ex2Lexer.mll"
              ( INT (int_of_string n) )
# 179 "ex2Lexer.ml"

  | 13 ->
let
# 23 "ex2Lexer.mll"
          b
# 185 "ex2Lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 23 "ex2Lexer.mll"
              ( BOOL (bool_of_string b) )
# 189 "ex2Lexer.ml"

  | 14 ->
# 24 "ex2Lexer.mll"
              ( EOF  )
# 194 "ex2Lexer.ml"

  | 15 ->
# 25 "ex2Lexer.mll"
              ( failwith ("Unknown Token: " ^ Lexing.lexeme lexbuf))
# 199 "ex2Lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

;;

