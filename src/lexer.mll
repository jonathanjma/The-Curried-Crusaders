{
open Parser

exception SyntaxError of string
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let id = (letter) (letter|digit|'_')*

let cal =  digit+
let joul =  digit+ '.' digit+
let rcp = letter+

rule read =
  parse 
  | white { read lexbuf }
  | "+" { PLUS }
  | "*" { TIMES }
  | "fk" { FORK }
  | "bs" { BOOLNEGATION }
  | "/" { DIVIDE }
  | "-" { SUBTRACT }
  | "~" { UNEGATION }
  | "::" { CONS }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "true" { TRUE }
  | "false" { FALSE }
  | "[" { LBRAC }
  | "]" { RBRAC }
  | ">" { GREATER }
  | "<" { LESS }
  | ">=" { GEQ }
  | "<=" { LEQ }
  | "=" { EQUAL }
  | "%" { MOD }
  | "," { COMMA }
  | "PIE" { PIE }
  | "cook" { COOK }
  | "curry" { CURRY }
  | "print" { PRINT }
  | "println" { PRINTLN }
  | "let" { LET }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | id { ID (Lexing.lexeme lexbuf) }
  | "\"" { read_string (Buffer.create 17) lexbuf }
  | cal { CAL (int_of_string (Lexing.lexeme lexbuf)) }
  | joul { JOUL (float_of_string (Lexing.lexeme lexbuf)) }
  | "()" {UNIT}
  | eof { EOF }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }

and read_string buf =
  parse
  | "\"" { RCP (Buffer.contents buf) }
  | [^ '"' '\\']+ 
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }
