exception MLFailure of string

type binop = 
  Plus 
| Minus 
| Mul 
| Div 
| Eq 
| Ne 
| Lt 
| Le 
| And 
| Or          
| Cons

type expr =   
  Const of int 
| True   
| False      
| NilExpr
| Var of string    
| Bin of expr * binop * expr 
| If  of expr * expr * expr
| Let of string * expr * expr 
| App of expr * expr 
| Fun of string * expr    
| Letrec of string * expr * expr
	
type value =  
  Int of int		
| Bool of bool          
| Closure of env * string option * string * expr 
| Nil                    
| Pair of value * value     

and env = (string * value) list

let binopToString op = 
  match op with
      Plus -> "+" 
    | Minus -> "-" 
    | Mul -> "*" 
    | Div -> "/"
    | Eq -> "="
    | Ne -> "!="
    | Lt -> "<"
    | Le -> "<="
    | And -> "&&"
    | Or -> "||"
    | Cons -> "::"

let rec valueToString v = 
  match v with 
    Int i -> 
      Printf.sprintf "%d" i
  | Bool b -> 
      Printf.sprintf "%b" b
  | Closure (evn,fo,x,e) -> 
      let fs = match fo with None -> "Anon" | Some fs -> fs in
      Printf.sprintf "{%s,%s,%s,%s}" (envToString evn) fs x (exprToString e)
  | Pair (v1,v2) -> 
      Printf.sprintf "(%s::%s)" (valueToString v1) (valueToString v2) 
  | Nil -> 
      "[]"

and envToString evn =
  let xs = List.map (fun (x,v) -> Printf.sprintf "%s:%s" x (valueToString v)) evn in
  "["^(String.concat ";" xs)^"]"

and exprToString e =
  match e with
      Const i ->
        Printf.sprintf "%d" i
    | True -> 
        "true" 
    | False -> 
        "false"
    | Var x -> 
        x
    | Bin (e1,op,e2) -> 
        Printf.sprintf "%s %s %s" 
        (exprToString e1) (binopToString op) (exprToString e2)
    | If (e1,e2,e3) -> 
        Printf.sprintf "if %s then %s else %s" 
        (exprToString e1) (exprToString e2) (exprToString e3)
    | Let (x,e1,e2) -> 
        Printf.sprintf "let %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 
    | App (e1,e2) -> 
        Printf.sprintf "(%s %s)" (exprToString e1) (exprToString e2)
    | Fun (x,e) -> 
        Printf.sprintf "fun %s -> %s" x (exprToString e) 
    | Letrec (x,e1,e2) -> 
        Printf.sprintf "let rec %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 

(*********************** Some helpers you might need ***********************)

let rec fold f base args = 
  match args with [] -> base
    | h::t -> fold f (f(base,h)) t

let listAssoc (k,l) = 
  fold (fun (r,(t,v)) -> if r = None && k=t then Some v else r) None l

(*********************** Your code starts here ****************************)
(* lookup : 'a * ('a * 'b) list -> 'b option
* (lookup (x,evn)) looks up x in the environment and returs the value 
* associated with it
*)
let lookup (x,evn) = listAssoc (x,evn)

(* (eval (evn,e)) evaluates expression e based on then environment evn
* and returns the value
*)
let rec eval (evn,e) = 
  match e with
  |Const num -> Int num
  |True -> Bool true
  |False -> Bool false
  |Var x -> (match (lookup (x,evn)) with
	    |None -> raise (MLFailure ("Variable not bound: " ^ x))
	    |Some v -> v)
  (*computes Bin - binary operations on two expressions*)
  |Bin (e1,op,e2) -> let e1' = eval(evn,e1) in
	             let e2' = eval(evn,e2) in
	             (match (e1',op,e2') with
		      |(Int n1,Plus,Int n2) -> Int (n1+n2)
 		      |(Int n1,Minus,Int n2) -> Int (n1-n2)
		      |(Int n1,Mul,Int n2) -> Int (n1*n2)
		      |(Int n1,Div,Int n2) -> if (n2 = 0) then 
					      raise (MLFailure ("Cannot divide by 0"))
					      else Int (n1/n2)
		      |(Int n1,Eq,Int n2) -> Bool (n1 = n2)
		      |(Bool b1,Eq,Bool b2) -> Bool (b1 = b2)
		      |(Int n1,Ne,Int n2) -> if (n1 = n2) then (Bool false) else (Bool true)
		      |(Bool b1,Ne,Bool b2) -> if (b1 = b2) then (Bool false) else (Bool true)
		      |(Int n1,Lt,Int n2) -> Bool (n1<n2)
		      |(Int n1,Le,Int n2) -> Bool (n1<=n2)
		      |(Bool b1,And,Bool b2) -> Bool (b1&&b2)
		      |(Bool b1,Or,Bool b2) -> Bool (b1||b2)
		      |_ -> raise (MLFailure ("Invalid operand"))) (*operands don't match pattern - invalid*)
  |If (p,t,f) -> let p_eval = eval (evn,p) in
		(*If p_eval is true then evaluates t else evaluates f*)
		(match p_eval with
		|Bool b -> if b then (eval (evn,t)) else (eval (evn,f))
		|_ -> raise (MLFailure ("Boolean expected in If-statement")))
  |Let (b,e1,e2) -> let e1' = eval(evn,e1) in (*evaluates e1, adds it to evn along with b as*)
		let evn' = (b,e1')::evn in     (*its string, then uses this new env to evaluate e2*)
		eval(evn',e2)
  |Letrec (b,e1,e2) -> let e1' = eval(evn,e1) in
		(*if e1 evaluates to a closure then it passes another closure*)
		(*with Some b as its name*)
		(match e1' with    
		|Closure(en,_,va,e) -> eval((b,Closure(en,Some b,va,e))::evn,e2)
		|_ -> eval ((b,e1')::evn,e2))
  |Fun (x,e) -> Closure (evn,None,x,e)
  |App (e1,e2) ->  let e2' = eval(evn,e2) in
		   let e1' = eval(evn,e1) in
			(*If the closure has Some f as its name, then it adds the name its current
			 state to the environment so that it can be called recursively*)
			(match e1' with
		  	|Closure (en,None,var,exp) -> eval(((var,e2')::en),exp)
			|Closure (en,Some f,var,exp) -> let evn2 = ((f,e1')::(var,e2')::en) in
							eval(evn2,exp)
			|_ -> raise (MLFailure ("Invalid operand in APP")))
		  
