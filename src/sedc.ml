open Printf
open Utils
open Prog

type instr = 
	| Const of int
	| Add
	| Minus
	| Mult

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
		in
		print_sedc xs
;;

let rec is_compilable p = 
	match p with
	| Plus(p1,p2) -> is_compilable p1 && is_compilable p2
	| Minus(p1,p2)	-> is_compilable p1 && is_compilable p2
	| Mult(p1,p2)	-> is_compilable p1 && is_compilable p2
	| Value(x)		-> true
	| _ ->	 false

;;



let rec compile p = 
	match p with
	| Plus(p1,p2) 	-> (compile p1)@(compile p2)@[Add];
	| Minus(p1,p2) 	-> (compile p1)@(compile p2)@[Minus];
	| Mult(p1,p2) 	-> (compile p1)@(compile p2)@[Mult];
	| Value(x)		-> [Const(x)]
	| _ -> printf "there was an error";[] 
	;;

<<<<<<< HEAD
let apply x s d = 
	match x with
	| Const(y) 	-> (s,x::d)
	| Add		-> (* we know that Add is of arity 2, so we try to pop 2 values of d (destack) if it doesn't work : error *)
					begin
					match d with
					| Const(b)::(Const(a)::d') ->
						let r = a+b in
						(Const(r)::s,d')
					| _ -> printf "wrong nb of args or wrong matching"; exit 1
					end
	| Minus		-> 
					begin
					match d with
					| Const(b)::(Const(a)::d') ->
						let r = a-b in
						(Const(r)::s,d')
					| _ -> printf "wrong nb of args or wrong matching"; exit 1
					end
	| Mult		-> 
					begin
					match d with
					| Const(b)::(Const(a)::d') -> let r = a*b in (Const(r)::s,d')
					| _ -> printf "wrong nb of args or wrong matching"; exit 1
					end

let rec execute s d =
	match s with
	| x::xs -> 	let (s',d') = apply x xs d in
				execute s' d'
	| []	-> 	match d with
				| [Const(y)]	-> printf "result : %d\n" y
				| _				-> printf "error in execution\n"
					
=======
>>>>>>> be5b3ba23525c22b1723338539747ee89b4c9263

