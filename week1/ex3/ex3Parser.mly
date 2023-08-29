%{
  open Ex3Syntax
  (* ここに書いたものは，ExampleParser.mliに入らないので注意 *)
%}
%token <int> INT
%token <bool> BOOL
%token <string> ID 
%token PLUS MINUS TIMES DIV
%token EQ LT
%token IF THEN ELSE
%token LPAR RPAR
%token SHARP
%token END
%token EOF


%start main
%type <Ex3Syntax.expr> main
%%

main:
expr EOF { $1 } 
| expr END { $1 } 

;


expr:
  | IF expr THEN expr ELSE expr  { EIf($2,$4,$6) }
  | compare_expr                 { $1 }
;

compare_expr:
  | arith_expr EQ arith_expr { EEqual($1, $3) }
  | arith_expr LT arith_expr { ECompare($1, $3) }
  | arith_expr               { $1 }
;

arith_expr:
  | arith_expr PLUS term_expr  { EBin(OpAdd, $1, $3) }
  | arith_expr MINUS term_expr { EBin(OpSub, $1, $3) }
  | term_expr                  { $1 }
;

term_expr:
  | term_expr TIMES atomic_expr { EBin(OpMul, $1, $3) }
  | term_expr DIV atomic_expr   { EBin(OpDiv, $1, $3) }
  | atomic_expr                 { $1 }
;

atomic_expr:
  | INT             { EValue (VInt $1) }
  | BOOL            { EValue (VBool $1) }
  | LPAR expr RPAR  { $2 }
  
;

var:
  | ID  { $1 } 
;
 
