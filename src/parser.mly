%{
open Ast
%}

%token <int> CAL
%token <float> JOUL
%token <string> ID
%token <string> RCP
%token <bool> BOOL
%token UNIT
%token TRUE
%token FALSE
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

%token LET
%token COOK
%token IN

%token CURRY

%token IF
%token THEN
%token ELSE

%nonassoc LET IN IF THEN ELSE CURRY 
%nonassoc TRUE FALSE PIE

(* operator precedence *)

%right FORK
%right CONS
%left EQUAL
%left GREATER LESS GEQ LEQ
%left PLUS SUBTRACT 
%left TIMES DIVIDE MOD
%right UNEGATION BOOLNEGATION PRINT PRINTLN

%start <Ast.expr> prog

%{
let rec desugar_list lst = 
  match lst with
  | [] -> Nil
  | h :: t -> Binop (Cons, h, desugar_list t)
%}

%%

prog:
  | e = expr; EOF { e }
  ;

expr:
  | v = value { v }
  | e1 = expr; op=bop ; e2 = expr { Binop (op, e1, e2) }
  | op=uop ; e = expr { Unop (op, e) }
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
| CONS { Cons }
| MOD { Mod }
| FORK { Fork }

%inline uop:
| BOOLNEGATION { Boolnegation }
| UNEGATION { Unegation }
| PRINT { Print }
| PRINTLN { Println }