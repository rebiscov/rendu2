open Printf
;;

type ident = string
;;

type prog =
	(* do nothing type ... ???why do we need it???*)
	| Unit
	(* function definition *)
	| Let of ident*prog*prog
	| Fun of ident*prog
	| Recfun of ident*prog
	(* access *)	
	| Id of ident
	| Value of int
	(* operators : must fail if the prog is a function of arity >= 1 *)	
	| Plus of prog*prog     
	| Mult of prog*prog			(* not parsed yet *)
	| Minus of prog*prog		(* not parsed yet *)
	(* fun application *)
	| App of prog*prog
	(* toolkit *)
	| Print of prog
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
	| Unit			->	printf "Unit()"
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
	| App(p1,p2)	-> 	printf "App(";
						print_prog' p1;
						printf ",";
						print_prog' p2;
						printf")"
	| Print(p1)		-> 	printf "Print(";
						print_prog' p1;
						printf ")"
	(* | _ -> printf "$not impl$" *)
in
print_prog' p;
printf "\n";;
