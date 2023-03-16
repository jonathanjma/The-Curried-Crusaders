%{
open Ast
%}

%token <int> CAL
%token <float> JOUL
%token <string> RCP
%token <char> ING
%token <bool> BOOL
%token TRUE
%token FALSE
%token <Ast.expr list> BOWL
%token LBRAC
%token RBRAC
%token COMMA
%token PLUS
%token FORK
%token PIE
%token TIMES
%token LPAREN
%token RPAREN
%token EOF

%token DOUBLE_QUOTE

%token LET
%token COOK
%token IN

(* lower precedence operators *)
%left FORK
%left PLUS
%left CONCAT
%left TIMES
(* higher precedence operators *)

%start <Ast.expr> prog

%%

prog:
  | e = expr; EOF { e }
  ;

expr:
  | i = CAL { Cal i }
  | f = JOUL { Joul f }
  | DOUBLE_QUOTE; s = RCP; DOUBLE_QUOTE { Rcp s }
  | c = ING { Ing c }
  | b = BOOL { Bool b }
  | PIE { Joul Float.pi }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | LBRAC; l = BOWL; RBRAC { Bowl l }
  | e1 = expr; PLUS; e2 = expr { Binop (Add, e1, e2) }
  | e1 = expr; FORK; e2 = expr { Binop (Fork, e1, e2) }
  | e1 = expr; TIMES; e2 = expr { Binop (Mult, e1, e2) }
  | LPAREN; e = expr; RPAREN { e }
  ;
