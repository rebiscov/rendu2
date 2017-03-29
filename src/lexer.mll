{
  open Parser;;        (* le type "token" est défini dans parser.mli *)
(* ce n'est pas à vous d'écrire ce fichier, il est engendré automatiquement *)
exception Eof;;
}

rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t' '\n'] { token lexbuf }
  | ";;" { EOP }
  | "let" { LET }
  | "in" { IN }
  | "print" { PRINT }
  | "+" { PLUS }
  | "*" { MULT }
  | ['1'-'9']['0'-'9']* as s {VAR (int_of_string s)}
  | ['a'-'z']['a'-'z']* as v {VAR (v) }
  | '(' {BLOCKSTART}
  | ')' {BLOCKEND}
  | eof { raise Eof}

