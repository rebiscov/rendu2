%{
(* --- préambule: ici du code Caml --- *)

open Prog   (* ou on definit le type expression *)

%}
/* description des lexèmes, ceux-ci sont décrits (par vous) dans lexer.mll */



%token <string> IDENT
%token <int> VALUE
%token EOP LET IN EQ  PLUS
%token PCLOSE POPEN

%nonassoc EOP
%nonassoc LET
%nonassoc IN
%nonassoc EQ
%nonassoc IDENT
%left PLUS



%start main             /* "start" signale le point d'entrée: */
                        /* c'est ici main, qui est défini plus bas */
%type <Prog.prog> main     /* on _doit_ donner le type associé au point d'entrée */
%%
    /* --- début des règles de grammaire --- */
                            /* à droite, les valeurs associées */
	
main:                       /* <- le point d'entrée (cf. + haut, "start") */
    prog EOP                { $1 }  /* on veut reconnaître un "expr" */
;

idents:
	| { [] }
	| IDENT idents { $1 :: $2 }

arg:
	| VALUE { Value($1) }
	| IDENT { Id($1) }
	/*| POPEN prog PCLOSE { $2 } */

args:
	| { [] }
	| arg args { $1::$2 }


prog:			    /* règles de grammaire pour les expressions */
  /*| LET IDENT EQ prog IN prog                  { Let($2,$4,$6)  }  -> this is a var definition*/       
  /*| LET IDENT IDENT EQ prog IN prog 			{ Let($2,Fun($3,$5),$7) }  -> this is a arity-1 function*/
  | LET IDENT idents EQ prog IN prog 		   { Let($2,List.fold_left (fun p v-> Fun(v,p)) $5 $3, $7) } /* this works for all*/
  | prog PLUS prog          { Plus($1,$3) }
  /* this is a funcall ... haha fun phonecall */
  | IDENT args { List.fold_left (fun f arg -> App(f,arg)) (Id($1)) $2 } /*List.fold_left (fun f arg -> App(f,arg)) $2 $1 */
  | VALUE { Value($1) }
;


