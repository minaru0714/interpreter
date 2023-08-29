%{
  open Ex2Syntax
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
%token REC
%token END
%token EOF


%nonassoc END
%nonassoc LET IN REC FUN ARROW
%nonassoc IF THEN ELSE
%left EQ LT
%left PLUS MINUS 
%left TIMES DIV
%nonassoc LPAR RPAR



%start main
%type <Ex2Syntax.expr> main

%start command
%type <Ex2Syntax.command> command
%%



main:
 |expr EOF { $1 }
 | expr END { $1 } 


command:
  | expr END { CExp $1 }
  | LET var EQ expr END { CLet ($2, $4) }
  | LET REC var EQ FUN var ARROW expr END { CRecFun ($3,$6,$8) } 
;
;

expr:
  | LET var EQ expr IN expr      { ELet($2,$4,$6) }
  | LET REC var var EQ expr IN expr    { ERecFun ($3,$4,$6,$8) }
  | IF expr THEN expr ELSE expr  { EIf($2,$4,$6) }
  | expr EQ expr { EEqual($1, $3) }
  | expr LT expr { ECompare($1, $3) }
  | expr PLUS expr  { EBin(OpAdd, $1, $3) }
  | expr MINUS expr { EBin(OpSub, $1, $3) }
  | expr TIMES expr { EBin(OpMul, $1, $3) }
  | expr  DIV expr   { EBin(OpDiv, $1, $3) }
  | FUN var ARROW expr { EFun($2,$4) }
  | expr apply_expr    {EApp ($1,$2)}
  | apply_expr { $1 }
;

apply_expr:
  | INT             { EValue (VInt $1) }
  | BOOL            { EValue (VBool $1) }
  | ID              { EVar $1 }
  | LPAR expr RPAR  { $2 }
;

var:
  | ID { $1 }
;