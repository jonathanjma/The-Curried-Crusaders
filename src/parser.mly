%{
open Ast
%}

%token <int> CAL
%token <float> JOUL
%token <string> ID
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
%token DIVIDE
%token SUBTRACT
%token UNEGATION

%token DOUBLE_QUOTE
%token SINGLE_QUOTE

%token LET
%token COOK
%token IN

%token CURRY

%token IF
%token THEN
%token ELSE

(* lower precedence operators *)

%left FORK

%left PLUS SUBTRACT 
%left TIMES DIVIDE




(* higher precedence operators *)

%start <Ast.expr> prog

%%

%inline bop:
| UNEGATION { Unegation }
| PLUS { Add }
| SUBTRACT { Subtract }
| TIMES { Mult }
| DIVIDE { Divide }

prog:
  | e = expr; EOF { e }
  ;


expr:
  | v = value { v }
  | e1 = expr; PLUS; e2 = expr { Binop (Add, e1, e2) }
  | e1 = expr; FORK; e2 = expr { Binop (Fork, e1, e2) }
  | e1 = expr; TIMES; e2 = expr { Binop (Mult, e1, e2) }
  | e1 = expr; DIVIDE; e2 = expr { Binop (Divide, e1, e2) }
  | e1 = expr; SUBTRACT; e2 = expr { Binop (Subtract, e1, e2) }
  | UNEGATION; e1 = expr { Unop (Unegation, e1) }
  | LPAREN; e = expr; RPAREN { e }
  | l_e = let_expr { l_e }
  | t = ternary_expr { t }
  ;
  

let_expr:
  | LET; n = ID; COOK; e1 = expr; IN; e2 = expr { LetExpression (n, e1, e2) }
  ;

ternary_expr:
  | IF; p = expr; THEN; e1 = expr; ELSE; e2 = expr {Ternary (p, e1, e2)}
  ;

value:
  | i = CAL { Cal i }
  | f = JOUL { Joul f }
  | c = RCP { Rcp c }
  // | s = Ing { Ing s }
  // | SINGLE_QUOTE; c = ING; SINGLE_QUOTE { Ing c }
  // | DOUBLE_QUOTE; s = RCP; DOUBLE_QUOTE { Rcp s }
  | iden = ID; { Identifier iden }

  | b = BOOL { Bool b }
  | PIE { Joul Float.pi }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | LBRAC; l = BOWL; RBRAC { Bowl l }
  | f = function_value {f}
  | a = function_app {a}
  ;
function_value:
  | CURRY; a = ID; COOK; e = expr { Function (a, e) }
  ;

function_app:
  | e1 = expr; e2 = expr {FunctionApp (e1, e2)}
;