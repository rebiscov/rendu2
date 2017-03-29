open Prog
open Hashtbl

let env = Hashtbl.create 1000;;

    let rec interpreter = fun prg ->
      match prg with
      | Let(name, prg1, prg2) -> Hashtbl.add env name (interpreter prg1);
                                 let prg' = interpreter prg2 in Hashtbl.remove env name; prg'
      | Plus(prg1, prg2) -> prg1
      | _ -> failwith("Not supported yet");;



      
      
    
        
