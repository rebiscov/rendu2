open Prog
open Sys
open Printf
open Interpreter
open Sedc

let lexbuf  = Lexing.from_channel stdin;;
let parse () = Parser.main Lexer.token lexbuf;;
  
(* we need to build an arg-parser *)
  
let main () =
	let inter = ref false in
	let debug = ref false in
	let sedc = ref false in
	for i = 0 to Array.length Sys.argv -1 do

		if Sys.argv.(i) = "--interpreter" || Sys.argv.(i) = "-i" then 
			inter := true

		else 
		if Sys.argv.(i) = "-debug" || Sys.argv.(i) = "-d" then 
			debug := true
		else
		if Sys.argv.(i) = "-machine" || Sys.argv.(i) = "-m" then
			sedc := true
		else ()
	done;
	if !inter then 
		let prog = parse() in 
		print_prog prog; 
		print_prog (launch_inter prog (!debug))
	else if !sedc then 
		let prog = parse() in
		if is_compilable prog then
			let s = compile prog in
			(*print_int (List.length s) ; *)
			print_sedc s ;
			execute s [];
		else
			print_string "the program is not compilable yet" 
	else
		()
	;;


main();;
    
