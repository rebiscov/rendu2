open Prog
open Printf
open Interpreter

let lexbuf  = Lexing.from_channel stdin;;
let parse () = Parser.main Lexer.token lexbuf;;

(* we need to build an arg-parser *)

let prog = parse () in
    print_prog prog; printf "\n"; print_prog (interpreter prog); printf "\n";;
    
