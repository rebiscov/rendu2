open Printf
open Hashtbl
;;

type ident = string
;;

type prog =
   	| Unit
   	(* just-in-time: token saying that this part of code can be executed by the sedc *)
   	| JIT of prog
	(* function definition *)
	| Clot of prog* (string,prog) Hashtbl.t
	| Let of ident*prog*prog
	| Fun of ident*prog
	| Recfun of ident*prog
	(* exceptions *)
	| Raise of int
	| Try of prog*prog
	(* references & imperative: *)
	| Semi of prog*prog
	| Ref of prog
	| Bang of ident
	| Reassign of ident*prog
	(* access *)	
	| Id of ident
	| Value of int
	| Refvalue of int ref
	(* operators : must fail if the prog is a function of arity >= 1 *)	
	| Plus of prog*prog     
	| Mult of prog*prog			(* not parsed yet *)
	| Minus of prog*prog		(* not parsed yet *)
	(* comparison operators *)
	| Eq of prog*prog
	| Neq of prog*prog
	| Greater of prog*prog
	| Greateq of prog*prog
	| Smaller of prog*prog
	| Smalleq of prog*prog
	(* fun application *)
	| App of prog*prog
	(* if statement *)
	(* compute prog1, if result is <> 0 then execute prog2 else execute prog3 *)
	(* be careful, an error must be raised if the output of prog1 is not an int *)
	| If of prog*prog*prog
	(* toolkit *)
	| Print
;;

let print_prog p = 
let rec print_prog' p = 
	match p with
	| Unit			-> 	printf "Unit"
	| JIT(p1)		-> 	printf "Just-in-time(";
						print_prog' p1;
						printf ")";
	| Let(id,p1,p2) -> 	printf "Let(%s," id;
						print_prog' p1;
						printf ",";
						print_prog' p2;
						printf ")"
	| Fun(id,p1) 	->	printf "Fun(%s," id;
						print_prog' p1;
						printf ")"
	| Recfun(id,p1) -> 	printf "Recfun(%s," id;
						print_prog' p1;
						printf ")"
	| Id(id)		-> 	printf "Id(%s)" id
	| Value(n)		->	printf "Value(%d)" n
	| Plus(p1,p2)	-> 	printf "Plus(";
						print_prog' p1;
						printf ",";
						print_prog' p2;
						printf ")"
	| Mult(p1,p2)	->	printf "Mult(";
						print_prog' p1;
						printf ",";
						print_prog' p2;
						printf ")"
	| Minus(p1,p2)	->	printf "Minus(";
						print_prog' p1;
						printf ",";
						print_prog' p2;
						printf ")"
	| Eq(p1,p2)	->	printf "Eq(";
						print_prog' p1;
						printf ",";
						print_prog' p2;
						printf ")"
	| Neq(p1,p2)	->	printf "Neq(";
						print_prog' p1;
						printf ",";
						print_prog' p2;
						printf ")"
	| Greater(p1,p2)	->	printf "Greater(";
						print_prog' p1;
						printf ",";
						print_prog' p2;
						printf ")"
	| Greateq(p1,p2)	->	printf "Greateq(";
						print_prog' p1;
						printf ",";
						print_prog' p2;
						printf ")"
	| Smaller(p1,p2)	->	printf "Smaller(";
						print_prog' p1;
						printf ",";
						print_prog' p2;
						printf ")"
	| Smalleq(p1,p2)	->	printf "Smalleq(";
						print_prog' p1;
						printf ",";
						print_prog' p2;
						printf ")"
	| App(p1,p2)	-> 	printf "App(";
						print_prog' p1;
						printf ",";
						print_prog' p2;
						printf")"
	| Print		-> 	printf "Print"
	| If(cond,p1,p2) -> printf "If(";
						print_prog' cond;
						printf ",";
						print_prog' p1;
						printf ",";
						print_prog' p2;
						printf ")"
                                                
	(* ref things *)
	| Ref(p1) -> 		printf "Ref(";
						print_prog' p1;
						printf ")";
	| Refvalue(e)	-> printf "RefValue(%d)" (!e)
	| Reassign(ident,p1) -> printf "Reassign(";
						print_string ident;
						printf ",";
						print_prog' p1;
						printf ")";
	| Bang(ident) ->	printf "Bang(%s)" ident;
	| Semi(p1,p2)	->	print_string "Semi(";
						print_prog' p1;
						print_string ",";
						print_prog' p2;
						print_string ")";
	| Try(p1,p2)	->	print_string "Try(";
						print_prog' p1;
						printf ",";
						print_prog' p2;
						print_string ")";
	| Raise(e)		-> printf "Raise(%d)" e
	| Clot(p,t)		-> printf "Clot(";
						print_prog' p;
						printf ",...)";
in
print_prog' p;
printf "\n";;
