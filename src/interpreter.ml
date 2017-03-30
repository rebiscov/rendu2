open Prog
open Hashtbl
open Printf

type expr =
  | Var of prog
  | Fun of prog * ident * (ident, expr)Hashtbl.t;;
   
let env: (ident, expr) Hashtbl.t = Hashtbl.create 1000;;

   
let rec interpreter prg env =
  match prg with
  | Let(name, Fun(id, prg1), prg2) -> begin try
                                          Hashtbl.add env name (Fun(prg1, id, env));
                                          let prg' = interpreter prg2 env in Hashtbl.remove env name; prg'
                                        with
                                        | Not_found -> printf "The key %s is not present in the hashtable\n" name; exit 1
                                        | e ->  printf "Unknown error in interpreter: Let fun definition: %s\n" (Printexc.to_string e); exit 1 end
  | Let(name, prg1, prg2) -> begin try
                                Hashtbl.add env name (Var(interpreter prg1 env));
                                let prg' = interpreter prg2 env in Hashtbl.remove env name; prg'
                           with
                           | Not_found -> printf "The key %s is not present in the hashtable\n" name; exit 1
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
      | Not_found -> printf "The key %s is not present in the hashtable\n" ident; exit 1
      | e -> printf "Unknown error in interpreter Id: %s" (Printexc.to_string e); exit 1 end

               
  | App(Id(id_fun), value) -> begin try
                                  let (f, id_value, env') = match Hashtbl.find env id_fun with Fun(f, id_value, env') -> (f, id_value, env') | _ -> failwith("Apply: not a function") in
                                  interpreter (Let(id_value, value, f)) env'
                                with
                                | Not_found -> printf "Can't find the function %s in the environment\n" id_fun; exit 1
                                | e -> printf "Application failed: unknown error: %s\n" (Printexc.to_string e); exit 1 end
               
  | _ -> failwith("Not supported yet");;
