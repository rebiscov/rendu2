open Prog
open Sys
open Printf
open Interpreter
open Sedc

let lexbuf_stdin  = Lexing.from_channel stdin;;
let parse_from_stdin () = Parser.main Lexer.token lexbuf_stdin;;

let parse filename = 
	let lexbuf = Lexing.from_channel (open_in filename) in
	Parser.main Lexer.token lexbuf ;;
  
let main () =
	let inter = ref false in
	let debug = ref false in
	let sedc = ref false in
	let filename = ref "" in
	for i = 1 to Array.length Sys.argv -1 do
		if Sys.argv.(i).[0] <> '-' then
			filename := Sys.argv.(i)
		else
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
		if !filename = "" then
			begin
			let prog = parse_from_stdin() in
			print_prog prog; 
			print_prog (launch_inter prog (!debug))
			end
		else
			begin
			let prog = parse (!filename) in 
			print_prog prog; 
			print_prog (launch_inter prog (!debug))
			end
	else if !sedc then 
		let prog = parse_from_stdin() in
		if is_compilable prog then
			let s = compile prog in
			print_sedc s ;
			execute s [] !debug;
		else
			print_string "the program is not compilable yet\n" 
	else
		()
	;;


main();;
    
