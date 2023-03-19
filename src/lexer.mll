{
open Parser

exception SyntaxError of string
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

let cal = '-'? digit+
let joul ='-'? digit+ '.' digit+
let ing = letter+
let rcp = letter+

rule read =
  parse 
  | white { read lexbuf }
  | "+" { PLUS }
  | "*" { TIMES }
  | "fk" { FORK }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "true" { TRUE }
  | "false" { FALSE }
  | "[" { LBRAC }
  | "]" { RBRAC }
  | "," { COMMA }
  | "PIE" { PIE }
  | "cook" {COOK}
  | "curry" {CURRY}
  | "let" {LET}
  | "in" {IN}
  | "if" {IF}
  | "then" {THEN}
  | "else" {ELSE}
  | "'" {SINGLE_QUOTE}
  | "\"" {DOUBLE_QUOTE}
  | rcp { RCP (Lexing.lexeme lexbuf) }
  | cal { CAL (int_of_string (Lexing.lexeme lexbuf)) }
  | joul { JOUL (float_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
