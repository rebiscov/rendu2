open Printf
;;

type ident = string
;;

type prog =
	(* function definition *)
	| Let of ident*prog*prog
	| Fun of ident*prog
	| Recfun of ident*prog
	(* references & imperative: *)
	| Semi of prog*prog
	| Ref of prog
	| Bang of ident
	| Reassign of ident*prog
	(* access *)	
	| Id of ident
	| Value of int
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

(* if i have time, can do a beauty printer of prog with line switches and indentation *)
let rec print_tabs n = 
	match n with
	| 0 -> ()
	| _ -> 	printf "  "; 
			print_tabs (n-1)
;;

let bprint_prog p = 
	printf "$not implemented$"
;;

let print_prog p = 
let rec print_prog' p = 
	match p with
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
	(* | _ -> printf "$not impl$" *)
in
print_prog' p;
printf "\n";;
