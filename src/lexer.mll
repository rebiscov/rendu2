{
  open Parser;;        (* le type "token" est défini dans parser.mli *)
(* ce n'est pas à vous d'écrire ce fichier, il est engendré automatiquement *)
exception Eof;;
}

rule token = parse    (* la "fonction" aussi s'appelle token *)
  | [' ' '\t' '\n'] { token lexbuf }
  | ";;" { EOP }
  | "let" { LET }
  | "=" { EQ }
  | "in" { IN }
  | "("  {POPEN}
  | ")"  {PCLOSE}
  | "+" { PLUS }
  | ['1'-'9']['0'-'9']* as s {VALUE (int_of_string s)}
  | ['a'-'z']['a'-'z']* as v {IDENT (v) }
  | eof { raise Eof}

