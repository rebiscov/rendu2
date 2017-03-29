open Prog
open Hashtbl

let env = Hashtbl.create 1000;;

let rec interpreter = fun prg ->
  match prg with
  | Let(name, prg1, prg2) -> Hashtbl.add env name (interpreter prg1);
                             let prg' = interpreter prg2 in Hashtbl.remove env name; prg'
  | Plus(prg1, prg2) -> let prg1' = interpreter prg1 in
                        let prg2' = interpreter prg2 in begin
                            match (prg1', prg2') with
                            | (Value(a), Value(b)) -> Value(a+b)
                            | _ -> failwith("Not a valid addition") end
  | Value(a) -> Value(a)
  | Var(ident) -> 
  | _ -> failwith("Not supported yet");;

