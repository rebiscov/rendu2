{
  open Parser;;        (* le type "token" est défini dans parser.mli *)
(* ce n'est pas à vous d'écrire ce fichier, il est engendré automatiquement *)
exception Eof;;
}

rule token = parse    (* la "fonction" aussi s'appelle token *)
  | [' ' '\t' '\n'] { token lexbuf }
  (* end of prog *)
  | ";;" { EOP } 
  (* mots cles reserves *)
  | ";"		{ SEMI }
  | "let" { LET }
  | "rec" { REC }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "print" { PRINT }
  | "prInt" { PRINT }
  | "fun"	{ FUN }
  | "ref" 	{ REF }
  | "try"	{ TRY }
  | "with"	{ WITH }
  | "raise"	{ RAISE }
  | "E"		{ EXCEP }
  (* reserved operators *)
  | "->"	{ ARROW }
  | "!"		{ BANG }
  | ":="	{ REASSIGN }
  (* comp operators *)
  | "<=" 	{ INFEQ }
  | "<"  	{ INF }
  | ">"  	{ GRE }
  | ">=" 	{ GREEQ }
  | "=" 	{ EQ }
  | "<>" 	{ NEQ }
  (* delimiters *)
  | "("  	{POPEN}
  | ")"  	{PCLOSE}
  (* binary ops *)
  | "+" { PLUS }
  | "*" { MULT }
  | "-" { MINUS }
  (* constants *)
  | ['0'-'9']['0'-'9']* as s {VALUE (int_of_string s)}
  | ['a'-'z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as v {IDENT (v) }
  | eof { raise Eof}

