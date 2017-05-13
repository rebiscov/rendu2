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
	let interm = ref false in
	let filename = ref "" in
	for i = 1 to Array.length Sys.argv -1 do
		if Sys.argv.(i).[0] <> '-' then
			filename := Sys.argv.(i)
		else
		if Sys.argv.(i) = "-interm" then
			interm := true
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
	
	let p = if !filename = "" then parse_from_stdin() else parse (!filename) in

	if !sedc then 
			begin
			let p' = prepare_jit p in    (* we prepare it*)
			let result = launch_inter p' (!debug) in
			print_string "result of the interpreter: \n";
			print_prog result;
			end


	else if !interm then
			begin
			if is_compilable p [] then
				let s = compile p in
				print_sedc s 
			else
				print_string "the program is not compilable yet\n" 
			end

	else
			begin
			print_prog p; 
			let r = (launch_inter p (!debug)) in
			print_prog r
			end
	;;


main();;
    
