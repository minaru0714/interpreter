%{
  open Ex3Syntax
%}

%token <int> INT
%token <bool> BOOL
%token <string> ID
%token LET IN 
%token PLUS MINUS TIMES DIV
%token EQ LT
%token IF THEN ELSE
%token LPAR RPAR
%token FUN ARROW
%token MATCH WITH OR END
%token REC
%token SEMI
%token EOF
%token COMMA 
%token CONS 
%token LBRACKET  RBRACKET 
%token AND

%nonassoc SEMI
%nonassoc LET IN REC FUN ARROW MATCH WITH OR END AND
%nonassoc IF THEN ELSE
%nonassoc COMMA
%left EQ LT
%right CONS
%left PLUS MINUS 
%left TIMES DIV
%nonassoc INT BOOL ID LPAR RPAR LBRACKET  RBRACKET 



%start main
%type <Ex3Syntax.expr> main

%start command
%type <Ex3Syntax.command> command
%%



main:
 |expr EOF { $1 }
 | expr SEMI { $1 } 


command:
  | expr SEMI { CExp $1 }
  | LET var EQ expr SEMI { CLet ($2, $4) }
  | LET REC var EQ FUN var ARROW expr SEMI { CRecFun ($3,$6,$8) } 
  | LET REC fun_list IN expr SEMI { CRecFunand $3 }
;
;

expr:
  | LET var EQ expr IN expr      { ELet($2,$4,$6) }
  | LET REC var var EQ expr IN expr    { ERecFun ($3,$4,$6,$8) }
  | LET REC fun_list IN expr SEMI { ERecFunand ($3, $5) }
  | IF expr THEN expr ELSE expr  { EIf($2,$4,$6) }
  | expr EQ expr { EEqual($1, $3) }
  | expr LT expr { ECompare($1, $3) }
  | expr PLUS expr  { EBin(OpAdd, $1, $3) }
  | expr MINUS expr { EBin(OpSub, $1, $3) }
  | expr TIMES expr { EBin(OpMul, $1, $3) }
  | expr  DIV expr   { EBin(OpDiv, $1, $3) }
  | FUN var ARROW expr { EFun($2,$4) }
  | expr apply_expr    {EApp ($1,$2)}
  | MATCH expr WITH pattern_expr END { EMatch ($2, $4) }
  | expr CONS expr { ECons ($1, $3) }
  | apply_expr { $1 }
;

fun_list:
  | var EQ FUN var ARROW expr { [($1, $4, $6)] }
  | var EQ FUN var ARROW expr AND fun_list { ($1, $4, $6) :: $8 }
;


expr_list:
  | expr { [$1] }
  | expr COMMA expr_list { $1 :: $3 }
;


pattern_expr :
  | pattern ARROW expr  { [($1, $3)] }
  | pattern ARROW expr OR pattern_expr { ($1, $3) :: $5 }
;

pattern:
  | INT     { PInt $1 }
  | BOOL    { PBool $1 }
  | ID      { PVar $1 }
  | LBRACKET RBRACKET { PNil }
  | LPAR pattern COMMA pattern_list RPAR { PTuple ($2 :: $4) }
  | pattern CONS pattern { PCons ($1, $3) }
;

pattern_list:
  | pattern { [$1] }
  | pattern COMMA pattern_list { $1 :: $3 }
;

apply_expr:
  | INT             { EValue (VInt $1) }
  | BOOL            { EValue (VBool $1) }
  | ID              { EVar $1 }
  | LPAR expr RPAR  { $2 }
  | LBRACKET  RBRACKET { ENil }
;



var:
  | ID { $1 }
;