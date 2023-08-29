{
    open Ex2Parser
}

let digit = ['0'-'9']
let space = ' ' | '\t' | '\r' | '\n' 
let bool = "true" | "false"

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
| "if"        { IF }
| "then"      { THEN }
| "else"      { ELSE }
| digit+ as n { INT (int_of_string n) }
| bool as b   { BOOL (bool_of_string b) }
| eof         { EOF  }
| _           { failwith ("Unknown Token: " ^ Lexing.lexeme lexbuf)}