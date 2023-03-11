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
%token EOF

%start <Ast.expr> prog

%%

prog:
  | e = expr; EOF { e }
  ;

expr:
  | i = CAL { Cal i }
  | f = JOUL { Joul f }
  | s = RCP { Rcp s }
  | c = ING { Ing c }
  | b = BOOL { Bool b }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | l = BOWL { Bowl l }
  ;
