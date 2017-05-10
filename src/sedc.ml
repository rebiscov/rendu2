open Printf
open Utils
open Prog




(* instruction type*)
type instr = 
	| CONST of int
	| ADD
	| MINUS
	| MULT
	| PRINT
	| LET of string
	| ENDLET
	| ACCESS of string

(* debug function for instr type *)
let rec print_sedc s =
	match s with
	| [] -> printf "\n"
	| x::xs ->
		let _ =
		match x with
		| CONST(y) -> printf "Const(%d);" y
		| ADD	-> printf "Add;"
		| LET(x) -> printf "Let(%s);" x
		| ENDLET	-> printf "Endlet;"
		| MINUS -> printf "Minus;"
		| MULT 	-> printf "Mult;"
		| PRINT	-> printf "Print;"
		| ACCESS(x)	-> printf "Access(%s);" x
		in
		print_sedc xs
;;

let rec access x env =
	match env with
	| (y,v)::env' -> if y=x then (true,v)
									else access x env'
	| []	-> (false,0)

(* check if the prog is compilable... *)
let rec is_compilable p env= 
	match p with
	| Plus(p1,p2) -> is_compilable p1 env && is_compilable p2 env
	| Minus(p1,p2)	-> is_compilable p1 env && is_compilable p2 env
	| Mult(p1,p2)	-> is_compilable p1 env && is_compilable p2 env
	| App(Print,p1)	-> is_compilable p1 env
	| Id(x)				-> let (b,_) = access x env in
								b;
	| Let(x,p1,p2)		-> is_compilable p1 env && is_compilable p2 ((x,0)::env)
	| Value(x)		-> true
	| _ ->	 false

;;

(* ... *)
let rec compile p = 
	match p with
	| Plus(p1,p2) 	-> (compile p1)@(compile p2)@[ADD];
	| Minus(p1,p2) 	-> (compile p1)@(compile p2)@[MINUS];
	| Mult(p1,p2) 	-> (compile p1)@(compile p2)@[MULT];
	| App(Print,p1)	-> (compile p1)@[PRINT];
	| Id(x)				-> [ACCESS(x)]
	| Let(x,p1,p2)		-> (compile p1) @ [LET(x)] @ (compile p2) @ [ENDLET];
	| Value(x)		-> [CONST(x)];
	| _ -> printf "there was an error";[] 
;;

(* we apply x (either a function or a constant) if it's a constant we push it on the d stack (destack), if it's a function we pop from d enough arguments to apply it, and we update the stacks by pushing the result in the stack s*)
let apply x s d env debug =
	let debugger str = if debug then printf str in
	let apply x s d env = 
	match x with
	| LET(id)		-> begin
							match d with
							| CONST(a)::d' -> (s,d',(id,a)::env)
							| _	-> printf "empty stack error";exit 1
							end

	| ENDLET		-> begin
						match env with
						| e::env' -> (s,d,env') 
						| _ ->	printf "environement error";exit 1
						end

	| ACCESS(id)	-> let (b,v) = access id env in
							if b then	(s,CONST(v)::d,env)
							else begin printf "id not in env error"; exit 1 end

	| CONST(y) 	-> (s,x::d,env)
	| ADD		-> (* we know that Add is of arity 2, so we try to pop 2 values of d (destack) if it doesn't work : error *)
					begin
					debugger "apply Add\n";
					match d with
					| CONST(b)::(CONST(a)::d') ->
						let r = a+b in
						(CONST(r)::s,d',env)
					| _ -> printf "empty stack error"; exit 1
					end
	| MINUS		-> 
					begin
					debugger "apply Minus\n";
					match d with
					| CONST(b)::(CONST(a)::d') ->
						let r = a-b in
						(CONST(r)::s,d',env)
					| _ -> printf "empty stack error"; exit 1
					end
	| MULT		-> 
					begin
					debugger "apply Mult\n";
					match d with
					| CONST(b)::(CONST(a)::d') -> let r = a*b in (CONST(r)::s,d',env)
					| _ -> printf "wrong nb of args or wrong matching"; exit 1
					end
	| PRINT		->
					begin
					debugger "apply Print\n";
					match d with
					| CONST(a)::d' ->  	printf "prInt : %d\n" a ; 
										(s,d,env)
					| _ -> printf "wrong nb of args or wrong matching"; exit 1
					end
	in 
	apply x s d env
;;

(* the execution: while the stack s is not empty, we apply it, if s is empty and d is not then there was a problem else we print the output *)
let rec execute s d env debug=
	match s with
	| x::xs -> 	let (s',d',env') = apply x xs d env debug in
				execute s' d' env' debug
	| []	-> 	match d with
				| [CONST(y)]	-> printf "result : %d\n" y
				| _				-> printf "error in execution\n"
					

