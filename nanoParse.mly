%{
(* See this for a tutorial on ocamlyacc 
 * http://plus.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamlyacc-tutorial/ *)
open Nano 
%}

%token <int> Num
%token <string> Id
%token TRUE FALSE
%token LET REC EQ IN
%token FUN ARROW
%token IF THEN ELSE
%token PLUS MINUS MUL DIV LT LE NE AND OR
%token LPAREN RPAREN
%token EOF

%start exp 
%type <Nano.expr> exp

%nonassoc LET FUN IF 
%left OR
%left AND
%left EQ LT LE NE
%left PLUS MINUS
%left MUL DIV
%nonassoc Num Id TRUE FALSE LPAREN RPAREN
%left APP
%%

exp: 
    |TRUE	{ True }
    |FALSE	{ False }
    |Num	{ Const $1 }
    |Id 	{ Var $1 }
    |LPAREN exp RPAREN		{ $2 }
    |exp exp	%prec APP	{ App ($1,$2) }
    |exp PLUS exp		{ Bin ($1,Plus,$3) }
    |exp MINUS exp		{ Bin ($1,Minus,$3) }
    |exp MUL exp 		{ Bin ($1,Mul,$3) }
    |exp DIV exp		{ Bin ($1,Div,$3) }
    |exp EQ exp			{ Bin ($1,Eq,$3) }
    |exp LT exp			{ Bin ($1,Lt,$3) }
    |exp LE exp 		{ Bin ($1,Le,$3) }
    |exp NE exp			{ Bin ($1,Ne,$3) }
    |exp AND exp 		{ Bin ($1,And,$3) }
    |exp OR exp			{ Bin ($1,Or,$3) }
    |IF exp THEN exp ELSE exp	{ If ($2,$4,$6) }
    |LET Id EQ exp IN exp	{ Let ($2,$4,$6) }
    |LET REC Id EQ exp IN exp	{ Letrec ($3,$5,$7) }
    |FUN Id ARROW exp		{ Fun ($2,$4) }

