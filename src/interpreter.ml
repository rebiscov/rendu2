open Prog
open Hashtbl
open Printf

type expr =
  | Var of prog
  | Fun of prog * ident * (ident, expr)Hashtbl.t;;
   

let rec get_prg prg = (* Get the prg of a function and the arguments*)
  match prg with
  | Prog.Fun(x, prg') -> let (l, prg'') = get_prg prg' in Printf.printf "Added key %s to the arguments\n" x; (x::l, prg'')
  | _ -> ([], prg);;

let rec get_id prg = (* Get the id of a function  *)
  match prg with
  | App(Id(f), _) -> f
  | App(x, _) -> get_id x
  | Id(f) -> f
  | _ -> Printf.printf "Can not find a ID for the application of the function\n"; exit 1;;

let rec apply prg args env = (* Add to the environment the arguments  *)
  match args with
  | [] -> env
  | a::r -> begin
      match prg with
      | App(x, prg') -> Hashtbl.add env a (Var(prg')); Printf.printf "Added key %s to env\n" a ;apply x r env
      | _ -> Printf.printf "Problem in application, too many arguments are given to the function\n"; exit 1 end;;
        

  
let rec interpreter prg env =
  match prg with
  | Let(name, Fun(id, prg1), prg2) -> begin try
                                          Hashtbl.add env name (Fun(Prog.Fun(id,prg1), id, env)); (* Faire cloture deepcopy *)
                                          let prg' = interpreter prg2 env in Hashtbl.remove env name; prg'
                                        with
                                        | Not_found -> printf "Let function:the key %s is not present in the hashtable\n" name; exit 1
                                        | e ->  printf "Unknown error in interpreter: Let fun definition: %s\n" (Printexc.to_string e); exit 1 end

  | Let(name, prg1, prg2) -> begin try
                                Hashtbl.add env name (Var(interpreter prg1 env));
                                let prg' = interpreter prg2 env in Hashtbl.remove env name; prg'
                               with
                               | Not_found -> printf "Let variables:the key %s is not present in the hashtable\n" name; exit 1
                               | e -> printf "Unknown error in interpreter: Let definition: %s\n" (Printexc.to_string e); exit 1 end
                           
  | Plus(prg1, prg2) -> let prg1' = interpreter prg1 env in
                        let prg2' = interpreter prg2 env in begin
                            match (prg1', prg2') with
                            | (Value(a), Value(b)) -> Value(a+b)
                            | _ -> failwith("Not a valid addition") end
                                                      
  | Mult(prg1, prg2) -> let prg1' = interpreter prg1 env in
                        let prg2' = interpreter prg2 env in begin
                            match (prg1', prg2') with
                            | (Value(a), Value(b)) -> Value(a*b)
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

  | App(x, value) -> let (f, env') = begin
                         match (Hashtbl.find env (get_id x)) with
                         | Fun(f, _, env') -> (f, env')
                         | _ -> Printf.printf "Not a function stored\n"; exit 1 end in
                     let (args, f_prg) = get_prg f in
                     let env'' = apply prg args env' in interpreter f_prg env''
                     
  | _ -> failwith("Not supported yet");;
