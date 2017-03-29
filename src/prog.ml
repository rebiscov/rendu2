type var = string

type prog = 
	| App of prog*prog
	| Fun of var*prog
	| Let of ident*prog*prog
	| Value of int
	| Plus of prog*prog      (* /!\ Plus, Mult and Print must fail if the prog is a function of arity > 1 *)
	| Mult of prog*prog
	| Print of prog;;
