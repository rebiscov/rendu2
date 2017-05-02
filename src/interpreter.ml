open Prog
open Hashtbl
open Printf
open Utils

let clots = Hashtbl.create 1000;; (* contient les clotures des fonctions *)
let s = new_stack();; (* contient les arguments des fonctions *)
let s1 = new_stack();; (* pareil *)
let env: (ident, prog) Hashtbl.t = Hashtbl.create 1000;; (* environement contenant les variables, les fonctions *)
let exn = ref (false, 0);;
  
let make_cloture prg env funname debug = (* Fonction qui construit la cloture d'une fonction *)
  let vars = Hashtbl.create 100 in
  let clot = Hashtbl.create 100 in
  
  (* findvar nous donne le nom des arguments attendu par une fonction (prg), ces nom ne doivent pas etre ajoute a la cloture *) 
  let rec findvar prg =
    match prg with
    | Fun(id,prg') | Recfun(id, prg')->	
       Hashtbl.add vars id ();  (* on init cette table *)
       findvar prg'
       
    | _ -> ()
  in
  (* on ajoute a la cloture toutes les variables necessaires au bon fonctionnement de f mais 
	pas ses variables libres d'ou l'appel de findvar d'abord *)
  let rec findid prg =
    match prg with
    | Id(id) | Bang(id)->
       if debug then Printf.printf "  MC: treating %s..." id;
       if not (Hashtbl.mem vars id) then
         begin
           let v = Hashtbl.find env id in
           Hashtbl.add clot id v;
           if debug then Printf.printf "  %s added successfully !\n" id;
         end
       else
         if debug then Printf.printf "  %s was not added: it is one of the arguments\n" id
       
    | Let(_,prg1,prg2) | Plus(prg1,prg2) | Minus(prg1,prg2) | Mult(prg1,prg2) | App(prg1,prg2) | Eq(prg1, prg2) | Neq(prg1, prg2) | Smaller(prg1, prg2) | Smalleq(prg1, prg2) | Greater(prg1, prg2) | Greateq(prg1, prg2) | Semi(prg1, prg2) -> 
       findid prg1;
       findid prg2
    | Fun(_,prg1) | Recfun(_, prg1) | Ref(prg1) | Reassign(_, prg1) -> 
       findid prg1
    | Value(_) | Print  -> ()
    | If(prg1, prg2, prg3) ->
       findid prg1;
       findid prg2;
       findid prg3;
    | _ -> if debug then
             begin
               Printf.printf "Warning: in make_cloture, some pattern are not recognised :";
               print_prog prg
             end
        
  in
  Hashtbl.add vars funname ();
  findvar prg;
  findid prg;
  clot;;

let rec prepare_cloture prg = (* Fonction qui renome toutes les occurences d'un id en _id, ce qui permet de ne pas avoir de collisions lorsque q'une fonction prend en argument une fonction *)
  match prg with
  | Let(ident, prg1, prg2) ->
     let prg1' = prepare_cloture prg1 in
     let prg2' = prepare_cloture prg2 in
     Let("_"^ident, prg1', prg2')
  | Fun(ident, prg') ->
     let prg'' = prepare_cloture prg' in
     Fun("_"^ident, prg'')
  | Recfun(ident, prg') ->
     let prg'' = prepare_cloture prg' in
     Recfun("_"^ident, prg'')
  | Raise(n) ->
     prg
  | Try(prg1, n, prg2) ->
     let prg1' = prepare_cloture prg1 in
     let prg2' = prepare_cloture prg2 in
     Try(prg1', n, prg2')
  | Semi(prg1, prg2) ->
     let prg1' = prepare_cloture prg1 in
     let prg2' = prepare_cloture prg2 in
     Semi(prg1', prg2')
  | Ref(prg') ->
     let prg'' = prepare_cloture prg' in
     Ref(prg'')
  | Bang(ident) ->
     Bang("_"^ident)
  | Reassign(ident, prg') ->
     let prg'' = prepare_cloture prg' in
     Reassign("_"^ident, prg'')
  | Id(ident) ->
     Id("_"^ident)
  | Value(n) ->
     prg
  | Plus(prg1, prg2) ->
     let prg1' = prepare_cloture prg1 in
     let prg2' = prepare_cloture prg2 in
     Plus(prg1', prg2')
  | Mult(prg1, prg2) ->
     let prg1' = prepare_cloture prg1 in
     let prg2' = prepare_cloture prg2 in
     Mult(prg1', prg2')     
  | Minus(prg1, prg2) ->
     let prg1' = prepare_cloture prg1 in
     let prg2' = prepare_cloture prg2 in
     Minus(prg1', prg2')     
  | Eq(prg1, prg2) ->
     let prg1' = prepare_cloture prg1 in
     let prg2' = prepare_cloture prg2 in
     Eq(prg1', prg2')     
  | Neq(prg1, prg2) ->
     let prg1' = prepare_cloture prg1 in
     let prg2' = prepare_cloture prg2 in
     Neq(prg1', prg2')     
  | Greater(prg1, prg2) ->
     let prg1' = prepare_cloture prg1 in
     let prg2' = prepare_cloture prg2 in
     Greater(prg1', prg2')     
  | Greateq(prg1, prg2) ->
     let prg1' = prepare_cloture prg1 in
     let prg2' = prepare_cloture prg2 in
     Greateq(prg1', prg2')     
  | Smaller(prg1, prg2) ->
     let prg1' = prepare_cloture prg1 in
     let prg2' = prepare_cloture prg2 in
     Smaller(prg1', prg2')     
  | Smalleq(prg1, prg2) ->
     let prg1' = prepare_cloture prg1 in
     let prg2' = prepare_cloture prg2 in
     Smalleq(prg1', prg2')     
  | App(prg1, prg2) ->
     let prg1' = prepare_cloture prg1 in
     let prg2' = prepare_cloture prg2 in
     App(prg1', prg2')     
  | If(prg1, prg2, prg3) ->
     let prg1' = prepare_cloture prg1 in
     let prg2' = prepare_cloture prg2 in
     let prg3' = prepare_cloture prg3 in
     If(prg1', prg2', prg3')
  | Print ->
     prg
  | Unit ->
     prg
  | _ -> print_prog prg; failwith(": not supported in prepare_cloture")
;;
  
let add_cloture clot env = (* Ajoute dans cloture tous les variables/fonctions contenus dans les arguments passés à la fonction en les renommant  *)
  let rec add_cloture_aux prg =
    match prg with
    | Id(ident) | Bang(ident) ->
       if Hashtbl.mem env ident then
         begin
           let e = Hashtbl.find env ident in
           Hashtbl.add clot ("_"^ident) e (* On renomme *)
         end
    | Let(_,prg1,prg2) | Plus(prg1,prg2) | Minus(prg1,prg2) | Mult(prg1,prg2) | App(prg1,prg2) | Eq(prg1, prg2) | Neq(prg1, prg2) | Smaller(prg1, prg2) | Smalleq(prg1, prg2) | Greater(prg1, prg2) | Greateq(prg1, prg2) | Semi(prg1, prg2) -> 
       add_cloture_aux prg1; add_cloture_aux prg2
    | Fun(_,prg1) | Recfun(_, prg1) -> 
       add_cloture_aux prg1
    | Value(_) | Print -> ()
    | If(prg1, prg2, prg3) ->
       add_cloture_aux prg1; add_cloture_aux prg2; add_cloture_aux prg3
    | _ -> Printf.printf "add_cloture: following prog not supported:"; print_prog prg; exit 1 in
  while not (empty s1) do
    let prg = pop s1 in
    add_cloture_aux prg
  done;;
  

let launch_inter prg debug =
  let debugger e p = if debug then begin print_string e; print_prog p end;
  in 
  let rec interpreter prg env =
    match prg with
    | Let(name, Fun(id, prg1), prg2) -> 
       let f = Fun(id,prg1) in
       debugger ("Let: defining fun :"^name^" = ") f ;

       let clot = make_cloture f env name debug in
       Hashtbl.add env name f;
       Hashtbl.add clots name clot;
       let out = interpreter prg2 env in
       Hashtbl.remove env name;
       Hashtbl.remove clots name;

       debugger ("Let: removing function "^name^" = ") f;
       
       out
       
    | Let(name, Recfun(id, prg1), prg2) -> 
       let f = Recfun(id, prg1) in
       debugger ("Let: defining recfun "^name^" = ") f;
       let clot = make_cloture f env name debug in
       Hashtbl.add env name f;
       Hashtbl.add clot name f;
       Hashtbl.add clots name clot;
       let out = interpreter prg2 env in
       
       debugger ("Let: removing recfun "^name^" = ") f;
       Hashtbl.remove env name;
       Hashtbl.remove clots name;
       out

    | Let(name, Ref(prg1), prg2) -> 
       let prg1' = interpreter prg1 env in
       let n =
         begin
           match prg1' with
           | Value(m) -> m
           | _ -> Printf.printf "Let: ref -> prg1 not a Value\n"; exit 1
         end
       in
       debugger ("Let: defining ref :"^name^" = ") (Ref(prg1));

       Hashtbl.add env name (Refvalue(ref n));
       let out = interpreter prg2 env in

       debugger ("Let: removing reference "^name^" = ") (Ref(prg1));
       Hashtbl.remove env name;

       out

    | Let(name, prg1, prg2) -> 
       debugger ("Let: defining var : "^name^" = ") prg1;
       let prg1' = interpreter prg1 env in
       Hashtbl.add env name prg1';
       let prg2' = interpreter prg2 env in
       Hashtbl.remove env name;
       debugger ("Let: deleting var "^name^" = ") prg1;
       prg2'

    | Ref(prg') ->
       Ref(interpreter prg' env)
       
    | Bang(ident) ->
       if debug then Printf.printf "Bang '%s':" ident;
       let r = Hashtbl.find env ident in
       begin
         match r with
         | Refvalue(refe) -> Value(!refe)
         | _ -> failwith("Not a Refvalue")
       end

    | Reassign(ident, prg') ->
       if debug then Printf.printf "Reassign:";
       let prg'' = interpreter prg' env in
       let n = 
         begin
           match prg'' with
           | Value(m) -> m
           | _ -> failwith("Not a value")
         end
       in
       
       let r = Hashtbl.find env ident in
       begin
         match r with
         | Refvalue(refe) -> 
            refe := n;
            Value(n)
         | _ -> failwith("not a ref value stored")
       end
         
       
    | Plus(prg1, prg2) -> let prg1' = interpreter prg1 env in
                          let prg2' = interpreter prg2 env in
                          begin
                            match (prg1', prg2') with
                            | (Value(a), Value(b)) -> if debug then Printf.printf "Adding %d and %d\n" a b;
                                                      Value(a+b)
                            | _ ->
                               if debug then Printf.printf "Warning, couldn't compute an addition\n";
                               (Plus(prg1', prg2'))
                          end

    | Minus(prg1, prg2) -> let prg1' = interpreter prg1 env in
                           let prg2' = interpreter prg2 env in
                           begin
                             match (prg1', prg2') with
                             | (Value(a), Value(b)) -> if debug then Printf.printf "Substracting %d and %d\n" a b;
                                                       Value(a-b)
                             | _ ->
                                if debug then Printf.printf "Warning, couldn't compute a substraction\n";
                               (Minus(prg1', prg2'))
                           end                                                          
                           
    | Mult(prg1, prg2) -> let prg1' = interpreter prg1 env in
                          let prg2' = interpreter prg2 env in
                          begin
                            match (prg1', prg2') with
                            | (Value(a), Value(b)) -> if debug then Printf.printf "Multiplicating %d and %d\n" a b;
                                                      Value(a*b)
                            | _ ->
                               if debug then Printf.printf "Warning, couldn't compute a multiplication\n";
                               (Mult(prg1', prg2'))
                          end
                          
    | Value(a) -> Value(a)
                
    | Id(ident) ->
       begin
         if Hashtbl.mem env ident then (* Si on connait l'id... *)
           begin
	     let p = Hashtbl.find env ident in
             if debug then 
	       begin
		 Printf.printf "Id: changing id '%s' for prog: " ident;
             	 print_prog p
	       end;
	     match p with (* et si cet id est une fonction, on change d'environement *)
             | Fun(x,prg')->
	        if Hashtbl.mem clots ident then
                  let cloture = Hashtbl.find clots ident in
                  let clot = Hashtbl.copy cloture in
                  add_cloture clot env;
                  interpreter (Fun(x,prg')) clot
                  
                else
                  begin
                    if debug then Printf.printf "Id: the key %s is not present in clots\n" ident;
                    interpreter p env
                  end
	     | Recfun(x,prg') ->
	        if Hashtbl.mem clots ident then
                  let cloture = Hashtbl.find clots ident in
                  let clot = Hashtbl.copy cloture in
                  add_cloture clot env;
                  interpreter (Recfun(x,prg')) clot
                else
                  begin
                    Printf.printf "Id: the key %s is not present in clots\n" ident;
                    interpreter p env
                  end                
		
             | prg'' -> interpreter prg'' env
           end
         else (* Si on ne connait id, on n'y touche pas *)
           begin
             if debug then Printf.printf "Id: warning: var not found %s\n" ident ;
             prg
           end
       end
       
    | App(Print, x) -> let prg' = interpreter x env in (* Utile seulement pour Print, print est considéré comme une fonction *)
                       begin
                         match prg' with
                         | Value(a) -> Printf.printf "prInt %d\n" a;
                                       prg'
                         | _ -> Printf.printf "Print: not a value to print"; exit 1

                       end
                       
    | App(x, p) -> if debug then (* On applique une fonction, on met les arguments dans la pile *)
                     begin 
                       Printf.printf "App: pushing in the queue: ";
                       print_prog p
                     end ;
                   let p_clean = interpreter p env in
		   push s (prepare_cloture p_clean);
                   push s1 p_clean;
                   interpreter x env
                   
    | Fun(id, prg') ->
       if not (empty s) then (* Si il y a des arguments dans la pile... *)
         begin
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
         end
       else
         prg
         
    | Recfun(id,prg') ->
       if not (empty s) then
         begin
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
         end
       else
         prg
				
    | If(prg1, prg2, prg3) -> if debug then Printf.printf "If then else\n";
                              let prg1' = interpreter prg1 env in
                              if debug then Printf.printf "End condition\n";
                              if prg1' = (Value(1)) then
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
                    | _ -> Printf.printf "Not a comparison of integers !\n"; print_prog prga; print_prog prgb; exit 1

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
                       
    | Semi(prg1 ,prg2) ->
       let _ = interpreter prg1 env in
       interpreter prg2 env

    | Try(prg1, n, prg2) ->
       let prg1' = interpreter prg1 env in
       let (b, m) = !exn in
       if b && m = n then (* Si il y a eu une exception et que c'est la bonne... *)
         begin
           exn := (false, 0);
           interpreter prg2 env
         end
       else
         prg1'


    | Raise(n) -> if debug then
                    Printf.printf "Raise: raising exception '%d'\n" n;
                  exn := (true, n);
                  prg
                  

    | _ -> print_prog prg; failwith("Not supported yet")
  in
  interpreter prg env ;;

