open Prog
open Hashtbl
open Printf
open Utils

let clots = Hashtbl.create 1000;;
let s = new_stack();;
  
let make_cloture prg env funname =
  let vars = Hashtbl.create (100) in
  let clot = Hashtbl.create (100) in
  let rec findvar prg =
    match prg with
    | Fun(id,prg') ->	Hashtbl.add vars id ();  (* on init cette table *)
      		 			findvar prg'
	| Recfun(id,prg')->	Hashtbl.add vars id ();
						findvar prg'
    | _ -> ()
  in
  let rec findid prg =
    match prg with
    | Id(id) ->
       if not (Hashtbl.mem vars id) then
         let v = Hashtbl.find env id in
         Hashtbl.add clot id v
    | Let(_,prg1,prg2)
      | Plus(prg1,prg2)
      | Minus(prg1,prg2)
      | Mult(prg1,prg2)
      | App(prg1,prg2)
      -> findid prg1;
         findid prg2
    | Fun(_,prg1)
      | Recfun(_, prg1)
      -> findid prg1
    |_ -> ()
  in
  findvar prg;
  findid prg;
  clot;;

  
let launch_inter prg env debug =
  let rec interpreter prg env =
    match prg with
    | Let(name, Fun(id, prg1), prg2) -> begin try
                                            if debug then Printf.printf "Defining a function\n";
                                            let clot = make_cloture (Fun(id, prg1)) env name in
                                            let f = Fun(id,prg1) in
                                            Hashtbl.add env name f;
                                            Hashtbl.add clots name clot;
                                            print_prog f ;
                                            if debug then Printf.printf "Adding function %s to the environment\n" name;
                                            let prg' = interpreter prg2 env in
                                            Hashtbl.remove env name;
                                            Hashtbl.remove clots name;

                                            if debug then  Printf.printf "Deleting function %s name from the environment\n" name ;
                                            prg'
                                          with
                                          | Not_found -> printf "Let function: the key %s is not present in the hashtable\n" name; exit 1
                                          | e ->  printf "Unknown error in interpreter: Let fun definition: %s\n" (Printexc.to_string e); exit 1 end

    | Let(name, Recfun(id, prg1), prg2) -> begin
                                               if debug then Printf.printf "Defining a function\n";
                                               let clot = make_cloture (Recfun(id, prg1)) env name in
											   let f = Recfun(id,prg1) in
                                               Hashtbl.add env name f;
                                               Hashtbl.add clot name f;
                                               Hashtbl.add clots name clot;
                                               if debug then Printf.printf "Adding function %s to the environment\n" name;
                                               let prg' = interpreter prg2 env in
                                               try
											   Hashtbl.remove env name;
                                               Hashtbl.remove clots name;
                                               

                                               if debug then  Printf.printf "Deleting function %s name from the environment\n" name ;
                                               prg'
                                             with
                                             | Not_found -> printf "Let function: the key %s is not present in the hashtable\n" name; exit 1
                                             | e ->  printf "Unknown error in interpreter: Let fun definition: %s\n" (Printexc.to_string e); exit 1 end                                    

    | Let(name, prg1, prg2) -> begin try
                                   if debug then Printf.printf "Defining a variable\n";
                                   let prg1' = interpreter prg1 env in
                                   Hashtbl.add env name prg1';
                                   if debug then Printf.printf "Adding variable %s to the environment\n" name;
                                   let prg2' = interpreter prg2 env in
                                   Hashtbl.remove env name;
                                   if debug then Printf.printf "Deleting variable %s from the environment\n" name;
                                   prg2'
                                   
                                 with
                                 | Not_found -> printf "Let variables:the key %s is not present in the hashtable\n" name; exit 1
                                 | e -> printf "Unknown error in interpreter: Let definition: %s\n" (Printexc.to_string e); exit 1 end

                             
    | Plus(prg1, prg2) -> let prg1' = interpreter prg1 env in
                          let prg2' = interpreter prg2 env in
                          begin
                            match (prg1', prg2') with
                            | (Value(a), Value(b)) -> if debug then Printf.printf "Adding %d and %d\n" a b;
                                                      Value(a+b)
                            | _ -> Plus(prg1', prg2')
                          end

    | Minus(prg1, prg2) -> let prg1' = interpreter prg1 env in
                           let prg2' = interpreter prg2 env in
                           begin
                             match (prg1', prg2') with
                             | (Value(a), Value(b)) -> if debug then Printf.printf "Substracting %d and %d\n" a b;
                                                       Value(a-b)
                             | _ -> failwith("Not a valid addition")
                           end                                                          
                           
    | Mult(prg1, prg2) -> let prg1' = interpreter prg1 env in
                          let prg2' = interpreter prg2 env in
                          begin
                            match (prg1', prg2') with
                            | (Value(a), Value(b)) -> if debug then Printf.printf "Multiplicating %d and %d\n" a b;
                                                      Value(a*b)
                            | _ -> failwith("Not a valid multiplication")
                          end
    | Value(a) -> Value(a)
                
    | Id(ident) ->
       begin
         if Hashtbl.mem env ident then
           	begin
		   	let p = Hashtbl.find env ident in
            if debug then 
			 	begin
				Printf.printf "Changing id %s for prog\n" ident;
             	print_prog p
			 	end;
			 match p with
             	| Fun(x,prg')->	if Hashtbl.mem clots ident 
								then
                                	let cloture = Hashtbl.find clots ident in
                                	interpreter (Fun(x,prg')) cloture
                              	else
                                	begin
                                  	Printf.printf "Id: the key %s is not present in clots\n" ident;
                                  	exit 1
                                	end
				| Recfun(x,prg') -> 
									let clot = Hashtbl.find clots ident in
									interpreter (Recfun(x,prg')) clot
									
             	| prg'' -> interpreter prg'' env
           end
         else
           begin
             if debug then Printf.printf "Warning: var not found %s\n" ident ;
             prg
           end
       end
    | App(Print, x) -> let prg' = interpreter x env in
                       begin
                         match prg' with
                         | Value(a) -> Printf.printf "prInt %d\n" a;
                                       prg'
                         | _ -> failwith("Print: not a value to print")
                       end
                       

    | App(x, p) -> if debug then
                    	begin 
                    	Printf.printf "Pushing in the stack: ";
                    	print_prog p
                    	end ;
				    let p_clean = interpreter p env in
					push s p_clean;
                 	interpreter x env
                      
    | Fun(id, prg') -> 	
                    let e = pop s in
					if debug then
                        begin
                        Printf.printf "Pop value from stack: ";
                       	print_prog e
                        end;
                    Hashtbl.add env id e;
                    let out = interpreter prg' env in
                    Hashtbl.remove env id;
                    out
	| Recfun(id,prg') -> 	let e = pop s in
							if debug then
                         		begin
                           		Printf.printf "Pop value from stack: ";
                           		print_prog e
                         	end;
							Hashtbl.add env id e;
							let out = interpreter prg' env in
							Hashtbl.remove env id;
							out
						
    | If(prg1, prg2, prg3) -> if debug then Printf.printf "If then else\n";
                              if interpreter prg1 env = (Value(1)) then
                                begin if debug then
                                        Printf.printf "In if\n";
                                      interpreter prg2 env
                                end
                              else
                                begin
                                  if debug then Printf.printf "In else\n";
                                  interpreter prg3 env
                                end

    | Eq(a, b) -> let prga = interpreter a env in
                  let prgb = interpreter b env in
                  begin
                    match (prga, prgb) with
                    | (Value(x), Value(y)) when x = y -> if debug then Printf.printf "Equality of %d and %d\n" x y;
                                                         Value(1)
                    | (Value(x), Value(y)) -> if debug then Printf.printf "Non equality of %d and %d\n" x y;
                                              Value(0)
                    | _ -> Printf.printf "Not a comparison of integers !\n"; print_prog prga; print_prog prgb;
                           exit 1
                  end
                  
                  
    | Neq(a, b) -> let prga = interpreter a env in
                   let prgb = interpreter b env in
                   begin
                     match (prga, prgb) with
                     | (Value(x), Value(y)) when x <> y -> if debug then Printf.printf "Non equality of %d and %d\n" x y;
                                                           Value(1)
                     | _ -> Value(0)
                   end

    | Greater(a, b) -> let prga = interpreter a env in
                       let prgb = interpreter b env in
                       begin
                         match (prga, prgb) with
                         | (Value(x), Value(y)) when x > y -> if debug then Printf.printf "%d is greater than %d\n" x y;
                                                              Value(1)
                         | _ -> Value(0)
                       end

    | Greateq(a, b) -> let prga = interpreter a env in
                       let prgb = interpreter b env in
                       begin
                         match (prga, prgb) with
                         | (Value(x), Value(y)) when x >= y -> if debug then Printf.printf "%d is greater of or equals than %d\n" x y;
                                                               Value(1)
                         | _ -> Value(0)
                       end

    | Smaller(a, b) -> let prga = interpreter a env in
                       let prgb = interpreter b env in
                       begin
                         match (prga, prgb) with
                         | (Value(x), Value(y)) when x < y -> if debug then Printf.printf "%d is smaller than %d" x y;
                                                              Value(1)
                         | _ -> Value(0)
                       end                                              

    | Smalleq(a, b) -> let prga = interpreter a env in
                       let prgb = interpreter b env in
                       begin
                         match (prga, prgb) with
                         | (Value(x), Value(y)) when x <= y -> if debug then Printf.printf "%d is smaller or equals than %d" x y;
                                                               Value(1)
                         | _ -> Value(0)
                       end                                                                
    | _ -> failwith("Not supported yet")
  in
  interpreter prg env ;;

