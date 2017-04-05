open Prog
open Sys
open Printf
open Interpreter

let env: (ident, expr) Hashtbl.t = Hashtbl.create 1000;;
let lexbuf  = Lexing.from_channel stdin;;
let parse () = Parser.main Lexer.token lexbuf;;
  
(* we need to build an arg-parser *)
  
let main () =
  let inter = ref false in
  for i = 0 to Array.length Sys.argv -1 do
    if Sys.argv.(i) = "--interpreter" then inter := true
    else ()
  done;
  if !inter then let prog = parse() in print_prog prog; print_prog (interpreter prog env)
  else let prog = parse() in
       print_prog prog;;

  main();;
    
