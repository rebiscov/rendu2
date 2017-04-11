open Prog
open Hashtbl
open Printf

type expr =
  | Var of prog
  | Ref of int ref 
  | Func of prog * (ident, expr)Hashtbl.t;;
   

let rec get_prg prg debug = (* Get the prg of a function and the arguments*)
  match prg with
  | Prog.Fun(x, prg') -> let (l, prg'') = get_prg prg' debug in if debug then Printf.printf "Added key %s to the arguments\n" x; (x::l, prg'')
  | _ -> ([], prg);;

let rec get_id prg = (* Get the id of a function  *)
  match prg with
  | App(Id(f), _) -> f
  | App(x, _) -> get_id x 
  | Id(f) -> f
  | _ -> Printf.printf "Can not find a ID for the application of the function\n"; exit 1;;


        
let make_cloture prg env name debug =
  let (args, prg_f) = get_prg prg debug in
  let h = Hashtbl.create 10 in
  let clot = Hashtbl.create 100 in
  let rec list_args args =
    match args with
    | a::r -> if (not (Hashtbl.mem h a)) then (Hashtbl.add h a ()) else () ; list_args r
    | _ -> () in
  list_args args;
  let rec add_clot prg =
    match prg with
    | Id(s) when s = name -> ()
    | Id(s) when not (Hashtbl.mem h s) -> 
       if (Hashtbl.mem clot s) then 
         begin Hashtbl.remove clot s; Hashtbl.add clot s (Hashtbl.find env s) end
       else Hashtbl.add clot s (Hashtbl.find env s)
    | Plus(a, b) | Mult(a, b) | Minus(a, b) | Eq(a, b) | Neq(a, b) | Greater(a, b) | Greateq(a, b) | Smaller(a, b) | Smalleq(a, b) | App(a, b) | Let(_, a, b)-> add_clot a; add_clot b
    | If(a, b, c) -> add_clot a; add_clot b; add_clot c
    | Print(a) | Recfun(_, a) | Fun(_, a) -> add_clot a
    | Id(_) | Value(_) -> ()
    | _ -> Printf.printf "prog = "; print_prog prg;Printf.printf "add_clot: prg not supported, it may cause problem later\n"; () in
  add_clot prg;
  clot;;

let rec apply prg args env debug = (* Add to the environment the arguments  *)
  match args with
  | [] -> env
  | a::r -> begin
      match prg with (* my shit *)
      | App(x, prg') -> let prg'' = interpreter prg' env debug in Hashtbl.add env a (Var(prg'')); if debug then Printf.printf "Added key %s to env\n" a ; apply x r env debug
      | _ -> Printf.printf "Problem in application, too many arguments are given to the function\n"; exit 1 end
  
and interpreter prg env debug =
  match prg with
  | Let(name, Fun(id, prg1), prg2) -> begin try
                                          if debug then Printf.printf "Defining a function\n";
                                          let clot = make_cloture (Fun(id, prg1)) env name debug in
                                          Hashtbl.add clot name (Func(Prog.Fun(id, prg1),clot));
                                          Hashtbl.add env name (Func(Prog.Fun(id,prg1), clot));
                                          if (debug) then Printf.printf "Adding function %s to the environment\n" name;
                                          let prg' = interpreter prg2 env debug in
                                          Hashtbl.remove env name; if debug then  Printf.printf "Deleting function %s name from the environment\n" name ;
                                          prg'
                                        with
                                        | Not_found -> printf "Let function: the key %s is not present in the hashtable\n" name; exit 1
                                        | e ->  printf "Unknown error in interpreter: Let fun definition: %s\n" (Printexc.to_string e); exit 1 end
                                    
  | Let(name, Ref(prg1), prg2) -> let prg1' = interpreter prg1 env debug in begin
                                      match prg1' with 
                                      | Value(a) -> let r = ref a in Hashtbl.add env name (Ref(r))
                                      | _ -> failwith("You can't create a reference of a non int type !") end ;
                                                                              interpreter prg2 env debug

  | Let(name, prg1, prg2) -> begin try
                                 if debug then Printf.printf "Defining a variable\n";
                                 Hashtbl.add env name (Var(interpreter prg1 env debug));
                                 if debug then Printf.printf "Adding variable %s to the environment\n" name;
                                 let prg' = interpreter prg2 env debug in
                                 Hashtbl.remove env name;
                                 if debug then Printf.printf "Deleting variable %s from the environment\n" name;
                                 prg'
                                                                          
                               with
                               | Not_found -> printf "Let variables:the key %s is not present in the hashtable\n" name; exit 1
                               | e -> printf "Unknown error in interpreter: Let definition: %s\n" (Printexc.to_string e); exit 1 end

  | Reassign(id, prg') -> let prg'' = interpreter prg' env debug in
                          begin
                            match prg'' with
                            | Value(a) -> let r = ref a in
                                          Hashtbl.remove env id;
                                          Hashtbl.add env id (Ref(r))
                            | _ -> failwith("Reassign: not a reference stored") end ;
                          Unit
                          
  | Bang(id) -> begin                
                  match (Hashtbl.find env id) with
                  | Ref(r') -> Value(!r')
                  | _ -> failwith("Not a reference stored")  end 
                

                                 
  | Plus(prg1, prg2) -> let prg1' = interpreter prg1 env debug in
                        let prg2' = interpreter prg2 env debug in begin
                            match (prg1', prg2') with
                            | (Value(a), Value(b)) -> if debug then Printf.printf "Adding %d and %d\n" a b; Value(a+b)
                            | _ -> failwith("Not a valid addition") end

  | Minus(prg1, prg2) -> let prg1' = interpreter prg1 env debug in
                        let prg2' = interpreter prg2 env debug in begin
                            match (prg1', prg2') with
                            | (Value(a), Value(b)) -> if debug then Printf.printf "Substracting %d and %d\n" a b;Value(a-b)
                            | _ -> failwith("Not a valid addition") end                                                          
                                                      
  | Mult(prg1, prg2) -> let prg1' = interpreter prg1 env debug in
                        let prg2' = interpreter prg2 env debug in begin
                            match (prg1', prg2') with
                            | (Value(a), Value(b)) -> if debug then Printf.printf "Multiplicating %d and %d\n" a b; Value(a*b)
                            | _ -> failwith("Not a valid multiplication") end
  | Value(a) -> Value(a)
  | Id(ident) -> begin
      try 
        match Hashtbl.find env ident with
        | Var(p) -> p
        | _ -> failwith("Id: not a variable")
      with
      | Not_found -> printf "Id: the key %s is not present in the hashtable\n" ident; exit 1
      | e -> printf "Unknown error in interpreter Id: %s" (Printexc.to_string e); exit 1 end

  | App(x, value) -> if debug then begin Printf.printf "Application of function %s\n" (get_id x); print_prog value end;
                     let (f, env') = begin
                         match (Hashtbl.find env (get_id x)) with
                         | Func(f, env') -> (f, env')
                         | _ -> Printf.printf "Not a function stored\n"; exit 1 end in
                     let (args, f_prg) = get_prg f debug in
                     let env'' = apply prg args env' debug in interpreter f_prg env'' debug
                                                            
  | If(prg1, prg2, prg3) -> if debug then Printf.printf "If then else\n";
                            if interpreter prg1 env debug = (Value(1)) then begin if debug then Printf.printf "In if\n"; interpreter prg2 env debug end
                            else begin Printf.printf "In else\n"; interpreter prg3 env debug end

  | Eq(a, b) -> let prga = interpreter a env debug in
                let prgb = interpreter b env debug in begin
                    match (prga, prgb) with
                    | (Value(x), Value(y)) when x = y -> if debug then Printf.printf "Equality of %d and %d\n" x y; Value(1)
                    | (Value(x), Value(y)) -> if debug then Printf.printf "Non equality of %d and %d\n" x y; Value(0)
                    | _ -> Printf.printf "Not a comparison of integers !\n"; print_prog prga; print_prog prgb; exit 1 end
  
                                              
  | Neq(a, b) -> let prga = interpreter a env debug in
                 let prgb = interpreter b env debug in begin
                     match (prga, prgb) with
                     | (Value(x), Value(y)) when x <> y -> if debug then Printf.printf "Non equality of %d and %d\n" x y; Value(1)
                     | _ -> Value(0) end

  | Greater(a, b) -> let prga = interpreter a env debug in
                let prgb = interpreter b env debug in begin
                    match (prga, prgb) with
                    | (Value(x), Value(y)) when x > y -> if debug then Printf.printf "%d is greater than %d\n" x y; Value(1)
                    | _ -> Value(0) end

  | Greateq(a, b) -> let prga = interpreter a env debug in
                     let prgb = interpreter b env debug in begin
                         match (prga, prgb) with
                         | (Value(x), Value(y)) when x >= y -> if debug then Printf.printf "%d is greater of or equals than %d\n" x y; Value(1)
                         | _ -> Value(0) end

  | Smaller(a, b) -> let prga = interpreter a env debug in
                     let prgb = interpreter b env debug in begin
                         match (prga, prgb) with
                         | (Value(x), Value(y)) when x < y -> if debug then Printf.printf "%d is smaller than %d" x y; Value(1)
                         | _ -> Value(0) end                                              

  | Smalleq(a, b) -> let prga = interpreter a env debug in
                     let prgb = interpreter b env debug in begin
                         match (prga, prgb) with
                         | (Value(x), Value(y)) when x <= y -> if debug then Printf.printf "%d is smaller or equals than %d" x y; Value(1)
                         | _ -> Value(0) end                                              

  | Print(prg') -> let prg'' = interpreter prg' env debug in begin
                       match prg'' with 
                       | Value(a) -> Printf.printf "prInt %d\n" a; prg''
                       | _ -> failwith("prInt: not an integer !") end

                                                    
  | _ -> failwith("Not supported yet");;
