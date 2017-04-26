open Printf
open Utils
open Prog

type instr = 
	| Const of int
	| Add

let rec print_sedc s =
	match s with
	| [] -> printf "\n"
	| x::xs ->
		let _ =
		match x with
		| Const(y) -> printf "Const(%d);" y
		| Add	-> printf "Add;"
		in
		print_sedc xs
;;

let rec is_compilable p = 
	match p with
	| Plus(p1,p2) -> is_compilable p1 && is_compilable p2
	| Value(x)		-> true
	| _ ->	 false

;;



let rec compile p = 
	match p with
	| Plus(p1,p2) 	-> (compile p1)@(compile p2)@[Add];
	| Value(x)		-> [Const(x)]
	| _ -> printf "there was an error";[] 
	;;


