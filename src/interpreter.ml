open Prog
open Hashtbl
open Printf

let env = Hashtbl.create 1000;;

let rec interpreter = fun prg ->
  match prg with
  | Let(name, prg1, prg2) ->begin try
                             Hashtbl.add env name (interpreter prg1);
                             let prg' = interpreter prg2 in Hashtbl.remove env name; prg'
                           with 
                           | Not_found -> printf "The key %s is not present in the hashtable" name; failwith("Not Found")
                           | _ -> failwith("Unknown error in interpreter: Let definition") end
                          
  | Plus(prg1, prg2) -> let prg1' = interpreter prg1 in
                        let prg2' = interpreter prg2 in begin
                            match (prg1', prg2') with
                            | (Value(a), Value(b)) -> Value(a+b)
                            | _ -> failwith("Not a valid addition") end
  | Value(a) -> Value(a)
  | Id(ident) -> Hashtbl.find env ident
  | _ -> failwith("Not supported yet");;

