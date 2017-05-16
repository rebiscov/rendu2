open Prog
open Hashtbl
open Printf
open Utils
open Sedc

let env: (string, prog) Hashtbl.t = Hashtbl.create 1000;; (* environement contenant les variables, les fonctions *)
let exn = ref (false, 0);;  (* reference utilisee pour les exceptions *)
  
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
    | Id(id) | Bang(id)  ->
       if debug then Printf.printf " adding  %s to clot" id;
       if not (Hashtbl.mem vars id) then
         begin
           let v = Hashtbl.find env id in
           Hashtbl.add clot id v;
         end
       else
         if debug then Printf.printf "  %s was not added: it is one of the arguments\n" id   
   	| Reassign(id,p1) ->
		if not (Hashtbl.mem vars id) then
			let v = Hashtbl.find env id in
			Hashtbl.add clot id v;
		else
			()
	| Let(_,prg1,prg2) | Plus(prg1,prg2) | Minus(prg1,prg2) | Mult(prg1,prg2) | App(prg1,prg2) | Eq(prg1, prg2) | Neq(prg1, prg2) | Smaller(prg1, prg2) | Smalleq(prg1, prg2) | Greater(prg1, prg2) | Greateq(prg1, prg2) | Semi(prg1, prg2) -> 
       findid prg1;
       findid prg2
    | Fun(_,prg1) | Recfun(_, prg1) | Ref(prg1)  -> 
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
  
let launch_inter prg debug =
  let debugger e p = if debug then begin print_string e; print_prog p end;
  in 
  let rec interpreter prg env s=
    match prg with
    | JIT(p1)	->    (* ce token n'existe que si on est en mode interpreteur+sedc *)
       printf "launching sedc for: "; print_prog p1;
       let s = compile p1 in
       let v = execute s debug in
       Value(v)
    | Let(id, p1, p2)	->
		begin
		match p1 with
			| Fun(x,_)	-> 	let clot = make_cloture p1 env id debug in
       						debugger ("add fun : "^id^ " = ") p1 ;
							Hashtbl.add env id (Clot(p1,clot));
       						let out = interpreter p2 env s in
       						Hashtbl.remove env id;
       						debugger ("rem fun : "^id^" = ") p1;
       						out
			| Recfun(x,_)->	
      				 		debugger ("add rec : "^id^" = ") p1;
       						let clot = make_cloture p1 env id debug in
       						Hashtbl.add clot id (Clot(p1,clot));
       						Hashtbl.add env id (Clot(p1,clot));
       						let out = interpreter p2 env s in
       						debugger ("rem rec : "^id^" = ") p1;
       						Hashtbl.remove env id;
       						out
			| Ref(p1')	->	begin
							let p1'' = interpreter p1' env (new_stack()) in
							match p1'' with
								| Value(x) -> 
      				 			debugger ("add ref : "^id^" = ") p1;
								Hashtbl.add env id (Refvalue(ref x));
								let out = interpreter p2 env s in
								Hashtbl.remove env id;
       							debugger ("rem ref : "^id^" = ") p1;
								out
								| _ -> print_prog p1; exit 1
							end
			| _			-> begin
							let p1' = interpreter p1 env (new_stack())in
							match p1' with
							| Value(n)->
							Hashtbl.add env id p1';
      				 		debugger ("add var : "^id^" = ") p1';
							let out = interpreter p2 env s in
							Hashtbl.remove env id;
       						debugger ("rem var : "^id^" = ") p1';
							out
							| _ -> failwith ("what is that???")
							end
		end
(*
    | Let(id, Fun(x, prg1), prg2) -> 
       let f = Fun(x,prg1) in
       debugger ("add fun :"^id^ " = ") f ;
       let clot = make_cloture f env id debug in
       Hashtbl.add env id (Clot(f,clot));
       let out = interpreter prg2 env s in
       Hashtbl.remove env id;
       debugger ("rem fun :"^id^" = ") f;
       out
       
    | Let(id, Recfun(x, prg1), prg2) -> 
       let f = Recfun(x, prg1) in
       debugger ("add recfun "^id^" = ") f;
       let clot = make_cloture f env id debug in
       Hashtbl.add clot id f;
       Hashtbl.add env id (Clot(f,clot));
       let out = interpreter prg2 env s in
       debugger ("rem recfun "^id^" = ") f;
       Hashtbl.remove env id;
       out

    | Let(name, Ref(prg1), prg2) -> 
       let prg1' = interpreter prg1 env s in
       let n =
         begin
           match prg1' with
           | Value(m) -> m
           | _ -> Printf.printf "Let: ref -> prg1 not a Value\n"; exit 1
         end
       in
       debugger ("Let: defining ref :"^name^" = ") (Ref(prg1));

       Hashtbl.add env name (Refvalue(ref n));
       let out = interpreter prg2 env s in

       debugger ("Let: removing reference "^name^" = ") (Ref(prg1));
       Hashtbl.remove env name;

       out
    | Let(name, prg1, prg2) -> 
       debugger ("Let: defining var : "^name^" = ") prg1;
       let prg1' = interpreter prg1 env s in
       Hashtbl.add env name prg1';
       let prg2' = interpreter prg2 env s in
       Hashtbl.remove env name;
       debugger ("Let: deleting var "^name^" = ") prg1;
       prg2'
*)
    | Ref(prg') ->
       Ref(interpreter prg' env s)
       
    | Bang(id) ->
       	debugger ("Bang "^id^" : ") Unit;
	   	begin
			if Hashtbl.mem env id then
       			let r = Hashtbl.find env id in
         		match r with
         		| Refvalue(refe) -> Value(!refe)
         		| _ -> failwith("Not a Refvalue")
			else
				failwith("Refvalue not found")
       	end

    | Reassign(ident, prg') ->
       if debug then Printf.printf "Reassign:";
       let prg'' = interpreter prg' env s in
       let n = 
         begin
           match prg'' with
           | Value(m) -> m
           | _ -> failwith("Not a value")
         end
       in
      	if Hashtbl.mem env ident then 
       let r = Hashtbl.find env ident in
       begin
         match r with
         | Refvalue(refe) -> 
            refe := n;
            Value(n)
         | _ -> failwith("not a ref value stored")
       end
	   else
	   	failwith("reassing var not found")
         
    (* for Plus, Minus, Mult, we compute first prg2 then prg1 like caml *)   
    | Plus(prg1, prg2) -> 	let prg2' = interpreter prg2 env s in
							let prg1' = interpreter prg1 env s in
                          begin
                            match (prg1', prg2') with
                            | (Value(a), Value(b)) -> if debug then Printf.printf "Adding %d and %d\n" a b;
                                                      Value(a+b)
                            | _ ->
                               if debug then Printf.printf "Warning, couldn't compute an addition\n";
                               (Plus(prg1', prg2'))
                          end

    | Minus(prg1, prg2) -> let prg2' = interpreter prg2 env s in
                           let prg1' = interpreter prg1 env s in
						   begin
                             match (prg1', prg2') with
                             | (Value(a), Value(b)) -> if debug then Printf.printf "Substracting %d and %d\n" a b;
                                                       Value(a-b)
                             | _ ->
                                if debug then Printf.printf "Warning, couldn't compute a substraction\n";
                               (Minus(prg1', prg2'))
                           end                                                          
                           
    | Mult(prg1, prg2) -> let prg2' = interpreter prg2 env s in
                          let prg1' = interpreter prg1 env s in
                          begin
                            match (prg1', prg2') with
                            | (Value(a), Value(b)) -> if debug then Printf.printf "Multiplicating %d and %d\n" a b;
                                                      Value(a*b)
                            | _ ->
                               if debug then Printf.printf "Warning, couldn't compute a multiplication\n";
                               (Mult(prg1', prg2'))
                          end
                          
    | Value(a) -> Value(a)
                
    | Id(id) ->
       begin
         if Hashtbl.mem env id then (* Si on connait l'id... *)
           	begin
	     	let p = Hashtbl.find env id in
		 	debugger ("computing "^id^" = ") p;
	     	match p with (* et si cet id est une fonction, on change d'environement *)
            	| Clot(p',clot)	->	interpreter p' clot s
      			| Refvalue(n)	-> p
				| Value(n)		-> p
				| _ 			-> print_prog p;failwith ("err quest ce que ca peut bien etre...")
           	end
         else (* Si on ne connait id, on n'y touche pas *)
           	begin
		   		debugger ("var not found "^id) Unit;
				failwith("var not found "^id);
           	end
       end
       
    | App(Print, x) -> let prg' = interpreter x env s in (* Utile seulement pour Print, print est considéré comme une fonction *)
                       begin
                         match prg' with
                         | Value(a) -> Printf.printf "(inter) prInt : %d\n" a;
                                       prg'
                         | _ -> Printf.printf "Print: not a value to print"; exit 1

                       end
                       
    | App(p1, p2) ->
	   	debugger "app " p1 ;
	   	debugger "with " p2;
	   	begin
	   	match p2 with
		| Id(id)		-> 
						let p3 = Hashtbl.find env id in
						push s p3
	   	| Fun(id,p3) 
	   	| Recfun(id,p3) -> push s (Clot(p2,env))
	   	| _	-> let p2' = interpreter p2 env s in
			   push s (p2')
		end;
		let out = interpreter p1 env s in
		out
    | Recfun(id,prg')
    | Fun(id, prg') ->
       if not (empty s) then (* Si il y a des arguments dans la pile... *)
         begin
           let e = pop s in
		   debugger "popping value from stack" e;
           Hashtbl.add env id e;
           let out = interpreter prg' env s in
           Hashtbl.remove env id;
           out
         end
       else
	   	 	failwith ("stack empty: cannot apply function")
				
    | If(prg1, prg2, prg3) -> if debug then Printf.printf "If then else\n";
                              let prg1' = interpreter prg1 env s in
                              if debug then Printf.printf "End condition\n";
                              if prg1' = (Value(1)) then
                                begin if debug then
                                        Printf.printf "In if\n";
                                      interpreter prg2 env s
                                end
                              else
                                begin
                                  if debug then Printf.printf "In else\n";
                                  interpreter prg3 env s
                                end

    | Eq(a, b) -> let prga = interpreter a env s in
                  let prgb = interpreter b env s in
                  begin
                    match (prga, prgb) with
                    | (Value(x), Value(y)) when x = y -> if debug then Printf.printf "Equality of %d and %d\n" x y;
                                                         Value(1)
                    | (Value(x), Value(y)) -> if debug then Printf.printf "Non equality of %d and %d\n" x y;
                                              Value(0)
                    | _ -> Printf.printf "Not a comparison of integers !\n"; print_prog prga; print_prog prgb; exit 1

                  end
                  
                  
    | Neq(a, b) -> let prga = interpreter a env s in
                   let prgb = interpreter b env s in
                   begin
                     match (prga, prgb) with
                     | (Value(x), Value(y)) when x <> y -> if debug then Printf.printf "Non equality of %d and %d\n" x y;
                                                           Value(1)
                     | _ -> Value(0)
                   end

    | Greater(a, b) -> let prga = interpreter a env s in
                       let prgb = interpreter b env s in
                       begin
                         match (prga, prgb) with
                         | (Value(x), Value(y)) when x > y -> if debug then Printf.printf "%d is greater than %d\n" x y;
                                                              Value(1)
                         | _ -> Value(0)
                       end

    | Greateq(a, b) -> let prga = interpreter a env s in
                       let prgb = interpreter b env s in
                       begin
                         match (prga, prgb) with
                         | (Value(x), Value(y)) when x >= y -> if debug then Printf.printf "%d is greater of or equals than %d\n" x y;
                                                               Value(1)
                         | _ -> Value(0)
                       end

    | Smaller(a, b) -> let prga = interpreter a env s in
                       let prgb = interpreter b env s in
                       begin
                         match (prga, prgb) with
                         | (Value(x), Value(y)) when x < y -> if debug then Printf.printf "%d is smaller than %d" x y;
                                                              Value(1)
                         | _ -> Value(0)
                       end                                              

    | Smalleq(a, b) -> let prga = interpreter a env s in
                       let prgb = interpreter b env s in
                       begin
                         match (prga, prgb) with
                         | (Value(x), Value(y)) when x <= y -> if debug then Printf.printf "%d is smaller or equals than %d" x y;
                                                               Value(1)
                         | _ -> Value(0)
                       end
                       
    | Semi(prg1 ,prg2) ->
		debugger "applying first instruction of semi\n" prg1;
       let _ = interpreter prg1 env s in
	   	debugger "applying second instruction of semi\n" prg2;
       interpreter prg2 env s

    | Try(prg1, prg2) ->
       let prg1' = interpreter prg1 env s in
       let (b,m) = !exn in
       if b then (* Si il y a eu une exception et que c'est la bonne... *)
         begin
           exn := (false, 0);
           interpreter (App(prg2, Value(m))) env s
         end
       else
         prg1'


    | Raise(n) -> debugger "raise excep" (Raise(n));
                  exn := (true, n);
                  prg
                  
	| Clot(p1,clot)	-> interpreter p1 clot s
    | _ -> print_prog prg; failwith("Not supported yet")
  in
  interpreter prg env (new_stack());;

