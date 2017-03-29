%{
(* --- préambule: ici du code Caml --- *)

open Prog   (* ou on definit le type expression *)

%}
/* description des lexèmes, ceux-ci sont décrits (par vous) dans lexer.mll */



%token <string> IDENT
%token <int> VALUE
%token EOP LET IN EQ  PLUS

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

prog:			    /* règles de grammaire pour les expressions */
  | LET IDENT EQ prog IN prog                  { Let($2,$4,$6)  }
  | LET IDENT IDENT EQ prog IN prog    		   { Let($2,Fun($3,$5),$7) }
  | prog PLUS prog          { Plus($1,$3) }
  | IDENT { Id($1) }
  | VALUE { Value($1) }
;


