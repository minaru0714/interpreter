{
    open Week2ex1Parser
}

let digit = ['0'-'9']
let space = ' ' | '\t' | '\r' | '\n' 
let bool = "true" | "false"
let alpha = ['a'-'z' 'A'-'Z' '_' ] 
let ident = alpha (alpha | digit)* 

rule token = parse
| space+      { token lexbuf }
| "="         { EQ }
| '<'         { LT }
| '+'         { PLUS }
| '-'         { MINUS }
| '*'         { TIMES }
| '/'         { DIV }
| '('         { LPAR }
| ')'         { RPAR }
| '#'         { SHARP }
| ";;"        { END }
| "let"       { LET }
| "in"        { IN }
| "if"        { IF }
| "then"      { THEN }
| "else"      { ELSE }
| "fun"       { FUN }
| "->"        { ARROW }
| digit+ as n { INT (int_of_string n) }
| bool as b   { BOOL (bool_of_string b) }
| ident as n  { ID n }
| eof         { EOF }
| _           { failwith ("Unknown Token: " ^ Lexing.lexeme lexbuf)}
