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
%token BOWL
%token LBRAC
%token RBRAC
%token COMMA
%token PLUS
%token FORK
%token PRINT
%token PRINTLN
%token PIE
%token TIMES
%token CONS
%token LPAREN
%token RPAREN
%token EOF
%token DIVIDE
%token MOD
%token SUBTRACT
%token UNEGATION
%token GREATER
%token LESS
%token GEQ
%token LEQ
%token EQUAL
%token BOOLNEGATION

%token DOUBLE_QUOTE
%token SINGLE_QUOTE

%token LET
%token COOK
%token IN

%token CURRY

%token IF
%token THEN
%token ELSE

%token UNIT

(* lower precedence operators *)

%left FORK


%left EQUAL
%left GREATER LESS GEQ LEQ

%left PLUS SUBTRACT 
%left TIMES DIVIDE MOD

%right UNEGATION BOOLNEGATION



%left CONS

(* higher precedence operators *)

%start <Ast.expr> prog

%{
let rec desugar_list lst = 
  match lst with
  | [] -> Nil
  | h :: t -> Binop (Cons, h, desugar_list t)
%}

%%

%inline bop:
| PLUS { Add }
| SUBTRACT { Subtract }
| TIMES { Mult }
| DIVIDE { Divide }
| GREATER { Greater }
| LESS { Less }
| EQUAL { Equal }
| LEQ { Leq }
| GEQ { Geq }

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
  | e1 = expr; EQUAL; e2 = expr { Binop (Equal, e1, e2) }
  | e1 = expr; GEQ; e2 = expr { Binop (Geq, e1, e2) }
  | e1 = expr; LEQ; e2 = expr { Binop (Leq, e1, e2) }
  | e1 = expr; LESS; e2 = expr { Binop (Less, e1, e2) }
  | e1 = expr; GREATER; e2 = expr { Binop (Greater, e1, e2) }
  | e1 = expr; MOD; e2 = expr { Binop (Mod, e1, e2) }
  | BOOLNEGATION; e1 = expr { Unop (Boolnegation, e1) }
  | UNEGATION; e1 = expr { Unop (Unegation, e1) }
  | PRINT; e1 = expr { Unop (Print, e1) }
  | PRINTLN; e1 = expr { Unop (Println, e1) }
  | LPAREN; e = expr; RPAREN { e }
  | l_e = let_expr { l_e }
  | l_d = let_defn { l_d }
  | t = ternary_expr { t }

  ;
  

let_expr:
  | LET; n = ID; COOK; e1 = expr; IN; e2 = expr { LetExpression (n, e1, e2) }
  ;

let_defn:
  | LET; n = ID; COOK; e = expr {LetDefinition (n, e)}
  ;

ternary_expr:
  | IF; p = expr; THEN; e1 = expr; ELSE; e2 = expr { Ternary (p, e1, e2) }
  ;

value:
  | i = CAL { Cal i }
  | f = JOUL { Joul f }
  | c = RCP { Rcp c }
  | iden = ID; { Identifier iden }
  | b = BOOL { Bool b }
  | u = UNIT { Unit }
  | PIE { Joul Float.pi }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | LBRAC; elts = separated_list(COMMA, value); RBRAC { Bowl (desugar_list elts) }
  | f = function_value {f}
  | a = function_app {a}
  ;
function_value:
  | CURRY; a = ID; COOK; e = expr { Function (a, e) }
  ;

function_app:
  | e1 = expr; e2 = expr { FunctionApp (e1, e2) }
;