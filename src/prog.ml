open Printf

type ident = string


type prog = 
	| App of prog*prog
	| Fun of ident*prog
	| Let of ident*prog*prog
	| Value of int
	| Plus of prog*prog      (* /!\ Plus, Mult and Print must fail if the prog is a function of arity > 1 *)
	| Mult of prog*prog
	| Print of prog;;



let rec print_prog p = 
	match p with
	| Let(id,p1,p2) -> 	printf "Let(%s," id;
						print_prog p1;
						printf ",";
						print_prog p2;
						printf ")";
	| Value(n) 		->	printf "Value(%d)" n;
	| Plus(a,b) 	-> 	printf "Plus(";
						print_prog a;
						printf ",";
						print_prog b;
						printf ")";
	| _ -> printf "$not implemented$"
;;

