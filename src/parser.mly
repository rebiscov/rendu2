%{
(* --- préambule: ici du code Caml --- *)

open Prog   (* ou on definit le type expression *)

%}
/* description des lexèmes, ceux-ci sont décrits (par vous) dans lexer.mll */



%token <string> IDENT
%token <int> VALUE
%token EOP
/* reserved words */
%token LET REC IN IF THEN ELSE
/* binary comp */
%token NEQ EQ GREEQ GRE INF INFEQ
/* binary op */
%token PLUS MINUS MULT
/* delimiters */
%token PCLOSE POPEN

%nonassoc EOP
%nonassoc LET
%nonassoc IN
%nonassoc EQ
%nonassoc IDENT
%left PLUS



%start main 
%type <Prog.prog> main     
%%
	
main:                       
    prog EOP { $1 }  
	

idents:
	| { [] }
	| IDENT idents { $1 :: $2 }


arg:
	| VALUE { Value($1) }
	| IDENT { Id($1) }
	| POPEN prog PCLOSE { $2 }

/* phuncall huhuhu whatta joke */
access:
	| arg			{ $1 }
	| access arg  { App($1,$2) }


prog:
/* delimiters */

/* fun and var definitions */  
  | LET IDENT idents EQ prog IN prog 		   { Let($2,List.fold_left (fun p v-> Fun(v,p)) $5 $3, $7) } 
  | LET REC IDENT idents EQ prog IN prog     { Let($3,List.fold_left (fun p v -> Recfun(v,p)) $6 $4, $8) }
  | prog PLUS prog          { Plus($1,$3) }
  | access 	{ $1 }




/*  | IDENT args { List.fold_left (fun f arg -> App(f,arg)) (Id($1)) $2 } */


/* a shit lot of other shit */
/*| IF a COMP b THEN p1 ELSE p2  { If(Comp($3),$2,$4,$6,$8) } */

;


