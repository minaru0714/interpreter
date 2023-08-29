%{
  open Week2ex1Syntax

%}
%token <int> INT
%token <bool> BOOL
%token <string> ID
%token LET IN EQ
%token PLUS MINUS TIMES DIV
%token EQ LT
%token IF THEN ELSE
%token LPAR RPAR
%token FUN ARROW
%token SHARP
%token END
%token EOF

%left PLUS MINUS 
%left TIMES DIV


%start main
%type <Week2ex1Syntax.expr> main

%start command
%type <Week2ex1Syntax.command> command
%%



main:
 |expr EOF { $1 }
 | expr END { $1 } 


command:
  | expr END { CExp $1 }
  | LET var EQ expr END { CLet ($2, $4) }
;
;



expr:
  | LET var EQ expr IN expr      { ELet($2,$4,$6) }
  | IF expr THEN expr ELSE expr  { EIf($2,$4,$6) }
  | compare_expr                 { $1 }
;

compare_expr:
  | arith_expr EQ arith_expr { EEqual($1, $3) }
  | arith_expr LT arith_expr { ECompare($1, $3) }
  | arith_expr               { $1 }
;

arith_expr:
  | arith_expr PLUS factor_expr  { EBin(OpAdd, $1, $3) }
  | arith_expr MINUS factor_expr { EBin(OpSub, $1, $3) }
  | factor_expr                  { $1 }
;

factor_expr:
  | factor_expr TIMES atomic_expr { EBin(OpMul, $1, $3) }
  | factor_expr  DIV atomic_expr   { EBin(OpDiv, $1, $3) }
  | apply_expr                 { $1 }
;

apply_expr:
  | FUN var ARROW expr { EFun($2,$4) }
  | expr atomic_expr       {EApp ($1,$2)}
  | atomic_expr             { $1 }

atomic_expr:
  | INT             { EValue (VInt $1) }
  | BOOL            { EValue (VBool $1) }
  | ID              { EVar $1 }
  | LPAR expr RPAR  { $2 }

var:
  | ID { $1 }
;