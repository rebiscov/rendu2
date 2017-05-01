open Printf
open Utils
open Prog




(* instruction type*)
type instr = 
	| Const of int
	| Add
	| Minus
	| Mult
	| Print

(* debug function for instr type *)
let rec print_sedc s =
	match s with
	| [] -> printf "\n"
	| x::xs ->
		let _ =
		match x with
		| Const(y) -> printf "Const(%d);" y
		| Add	-> printf "Add;"
		| Minus -> printf "Minus;"
		| Mult 	-> printf "Mult;"
		| Print	-> printf "Print;"
		in
		print_sedc xs
;;

(* check if the prog is compilable... *)
let rec is_compilable p = 
	match p with
	| Plus(p1,p2) -> is_compilable p1 && is_compilable p2
	| Minus(p1,p2)	-> is_compilable p1 && is_compilable p2
	| Mult(p1,p2)	-> is_compilable p1 && is_compilable p2
	| App(Print,p1)	-> is_compilable p1
	| Value(x)		-> true
	| _ ->	 false

;;

(* ... *)
let rec compile p = 
	match p with
	| Plus(p1,p2) 	-> (compile p1)@(compile p2)@[Add];
	| Minus(p1,p2) 	-> (compile p1)@(compile p2)@[Minus];
	| Mult(p1,p2) 	-> (compile p1)@(compile p2)@[Mult];
	| App(Print,p1)	-> (compile p1)@[Print];
	| Value(x)		-> [Const(x)];
	| _ -> printf "there was an error";[] 
;;

(* we apply x (either a function or a constant) if it's a constant we push it on the d stack (destack), if it's a function we pop from d enough arguments to apply it, and we update the stacks by pushing the result in the stack s*)
let apply x s d debug =
	let debugger str = if debug then printf str in
	let apply x s d = 
	match x with
	| Const(y) 	-> (s,x::d)
	| Add		-> (* we know that Add is of arity 2, so we try to pop 2 values of d (destack) if it doesn't work : error *)
					begin
					debugger "apply Add\n";
					match d with
					| Const(b)::(Const(a)::d') ->
						let r = a+b in
						(Const(r)::s,d')
					| _ -> printf "wrong nb of args or wrong matching"; exit 1
					end
	| Minus		-> 
					begin
					debugger "apply Minus\n";
					match d with
					| Const(b)::(Const(a)::d') ->
						let r = a-b in
						(Const(r)::s,d')
					| _ -> printf "wrong nb of args or wrong matching"; exit 1
					end
	| Mult		-> 
					begin
					debugger "apply Mult\n";
					match d with
					| Const(b)::(Const(a)::d') -> let r = a*b in (Const(r)::s,d')
					| _ -> printf "wrong nb of args or wrong matching"; exit 1
					end
	| Print		->
					begin
					debugger "apply Print\n";
					match d with
					| Const(a)::d' ->  	printf "prInt : %d\n" a ; 
										(s,d)
					| _ -> printf "wrong nb of args or wrong matching"; exit 1
					end
	in 
	apply x s d
;;

(* the execution: while the stack s is not empty, we apply it, if s is empty and d is not then there was a problem else we print the output *)
let rec execute s d debug=
	match s with
	| x::xs -> 	let (s',d') = apply x xs d debug in
				execute s' d' debug
	| []	-> 	match d with
				| [Const(y)]	-> printf "result : %d\n" y
				| _				-> printf "error in execution\n"
					

