%{
(* --- préambule: ici du code Caml --- *)

open Prog   (* ou on definit le type expression *)

%}
/* description des lexèmes, ceux-ci sont décrits (par vous) dans lexer.mll */



%token <string> IDENT
%token <int> VALUE
/* %token EOP */
/* reserved words */
%token LET REC IN IF THEN ELSE PRINT REF FUN
/* reserved operators */
%token BANG REASSIGN ARROW
/* comparison operators */
%token NEQ EQ GREEQ GRE INF INFEQ

/* binary operators */
%token PLUS MINUS MULT

/* delimiters */
%token PCLOSE POPEN COMMA


%nonassoc REF FUN

/* reserved words */
/*%nonassoc EOP */
%nonassoc LET
%nonassoc IN
%nonassoc IDENT


%nonassoc IF
%nonassoc THEN
%nonassoc ELSE


/* reference things */

%nonassoc BANG
%nonassoc REASSIGN

/* comparison operators */
%nonassoc NEQ
%nonassoc EQ
%nonassoc GREEQ
%nonassoc GRE
%nonassoc INF
%nonassoc INFEQ

/* important priorities */
%left ARROW
%left PLUS MINUS
%left MULT
%left PRINT
%left COMMA


%start main 
%type <Prog.prog> main     
%%

main:                       
    prog COMMA COMMA { $1 }  
	

idents:
	| 			{ [] }
	| IDENT idents { $1 :: $2 }


/* here func is used as a general function: it can either be an arity 0 function (a constant) or a regular function */
func:
	| VALUE 			{ Value($1) }
	| IDENT 			{ Id($1) }
	| PRINT				{ Print }
	| BANG IDENT		{ Bang($2) }
	| POPEN prog PCLOSE { $2 }

/* phuncall huhuhu whatta joke */
access:
	| func			{ $1 }
	| access func  { App($1,$2) }

comp:
	| prog GREEQ prog 	{ Greateq($1,$3) }
	| prog GRE prog 	{ Greater($1,$3) }
	| prog INFEQ prog 	{ Smalleq($1,$3) }
	| prog INF prog		{ Smaller($1,$3) }
	| prog EQ prog		{ Eq($1,$3) }
	| prog NEQ prog		{ Neq($1,$3) }


prog:
/* fun and var definitions */  
	| LET IDENT idents EQ prog IN prog 		   	{ Let($2,List.fold_left (fun p v-> Fun(v,p)) $5 $3, $7) } 
	| LET REC IDENT idents EQ prog IN prog     	{ Let($3,List.fold_left (fun p v -> Recfun(v,p)) $6 $4, $8) }
	| FUN IDENT ARROW prog						{ Fun($2,$4) }
/*| prog COMMA prog							{ Let("_",$1,$3) } */
	| prog MULT prog							{ Mult($1,$3) }
	| prog PLUS prog          					{ Plus($1,$3) }
	| prog MINUS prog							{ Minus($1,$3) }
	| MINUS prog								{ Minus(Value(0),$2) }
	| access 									{ $1 }
	| IDENT REASSIGN prog						{ Reassign($1,$3) }
	| REF prog									{ Ref($2) }
	| IF comp THEN prog ELSE prog  				{ If($2,$4,$6) }
;


