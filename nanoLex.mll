{
  open Nano        (* nano.ml *)
  open NanoParse   (* nanoParse.ml from nanoParse.mly *)
}

rule token = parse
  |[' ' '\n' '\r' '\t'] 
		{ token lexbuf } 	
  | "("		{ LPAREN }
  | ")" 	{ RPAREN }
  | "true"	{ TRUE }
  | "false" 	{ FALSE }
  | "if"	{ IF }
  | "then" 	{ THEN }
  | "else"      { ELSE } 
  | "let" 	{ LET }
  | "rec" 	{ REC }
  | "="		{ EQ }
  | "in"	{ IN }
  | "fun"	{ FUN }
  | "->"	{ ARROW }
  | "+"		{ PLUS }
  | "-"		{ MINUS }
  | "*"  	{ MUL }
  | "/"		{ DIV }
  | "<" 	{ LT }
  | "<="	{ LE }
  | "!="	{ NE }
  | "&&"	{ AND }
  | "||"	{ OR }
  |['0'-'9']+ as num 
		{ Num(int_of_string num) }
  |['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9']* as str
		{ Id(str) }
  |  eof         { EOF }
  | _           { raise (MLFailure ("Illegal Character '"^(Lexing.lexeme lexbuf)^"'")) }
 
