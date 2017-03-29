%{
(* --- préambule: ici du code Caml --- *)

open Prog   (* ou on definit le type expression *)

%}
/* description des lexèmes, ceux-ci sont décrits (par vous) dans lexer.mll */

/*
%token <int> VAR       /* le lexème INT a un attribut entier */
%token AND XOR OR IMPL EQ NOT VNOT
%token BLOCKSTART BLOCKEND
%token EOL             /* retour à la ligne */

%right IMPL
%left EQ
%left XOR  	
%left OR  	/* associativité gauche: a+b+c, c'est (a+b)+c */
%left AND 
%nonassoc NOT
%nonassoc VNOT
*/


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
  | LET var EQ prog IN prog                  { Let(var,prog,prog)  }
  | prog PLUS prog          { Plus($1,$3) }
  | prog MULT prog			{ Mult($1,$3) }
;


