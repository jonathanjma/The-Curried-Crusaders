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
let ing = letter+
let rcp = letter+

rule read =
  parse 
  | white { read lexbuf }
  | "+" { PLUS }
  | "*" { TIMES }
  | "fk" { FORK }
  | "/" { DIVIDE }
  | "-" { SUBTRACT }
  | "~" { UNEGATION }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "true" { TRUE }
  | "false" { FALSE }
  | "[" { LBRAC }
  | "]" { RBRAC }
  | "," { COMMA }
  | "PIE" { PIE }
  | "cook" { COOK }
  | "curry" { CURRY }
  | "let" { LET }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | id { ID (Lexing.lexeme lexbuf) }
  | "\"" { read_string (Buffer.create 17) lexbuf }
  (* | "'" { read_char (Buffer.create 17) lexbuf } *)
  (*| ing { ING (String.get (Lexing.lexeme lexbuf) 0) }*) (* idk why char doesn't work... *)
  | cal { CAL (int_of_string (Lexing.lexeme lexbuf)) }
  | joul { JOUL (float_of_string (Lexing.lexeme lexbuf)) }
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
