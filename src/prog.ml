open Printf


type ident = string


type prog = 
	| App of prog*prog
	| Fun of ident*prog
	| Let of ident*prog*prog
	| Value of int
	| Plus of prog*prog      (* /!\ Plus, Mult and Print must fail if the prog is a function of arity > 1 *)
	| Mult of prog*prog
	| Print of prog
    | Id of ident;;

let rec print_prog p = 
	match p with
	| Let(id,p1,p2) -> 	printf "Let(%s," id;
						print_prog p1;
						printf ",";
						print_prog p2;
						printf ")"
	| Value(n)		->	printf "Value(%d)" n
	| Id(id)		-> 	printf "Id(%s)" id
	| Plus(p1,p2)	-> 	printf "Plus(";
						print_prog p1;
						printf ",";
						print_prog p2;
						printf ")"
	| _ -> printf "$not impl$";;