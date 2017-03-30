open Printf


type ident = string


type prog = 
<<<<<<< HEAD
	| App of prog*prog (* Application of a function *)
	| Fun of ident*prog (* Declaration of a function *)
=======
	| App of prog*prog
	| Fun of ident*prog
	| Recfun of ident*prog
>>>>>>> fa1673849102c50470bfa98783695bd71ea1ea66
	| Let of ident*prog*prog
	| Value of int
	| Plus of prog*prog      (* /!\ Plus, Mult and Print must fail if the prog is a function of arity > 1 *)
	| Mult of prog*prog
	| Print of prog
	| Id of ident;;


let print_prog p = 
let rec print_prog' p = 
	match p with
	| Let(id,p1,p2) -> 	printf "Let(%s," id;
						print_prog' p1;
						printf ",";
						print_prog' p2;
						printf ")"
	| Value(n)		->	printf "Value(%d)" n
	| Id(id)		-> 	printf "Id(%s)" id
	| Plus(p1,p2)	-> 	printf "Plus(";
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
	| App(p1,p2)	-> 	printf "App(";
						print_prog' p1;
						printf ",";
						print_prog' p2;
						printf")"
	| _ -> printf "$not impl$"
in
print_prog' p;
printf "\n";;
