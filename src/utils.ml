open Prog;;
  
let new_stack () =
  ref [];;

let rec pop s =
  match !s with
  | a::r -> s := r; a
  | [] -> failwith("You are trying to pop an empty stack")

let push s a =
  s := a::!s;;

let empty s =
  !s = [];;

let is_fun prg =
  match prg with
  | Fun(_) | Recfun(_) ->
     true
  | _ ->
     false;;

let is_id prg =
  match prg with
  | Id(_) ->
     true
  | _ ->
     false;;
  
let get_id prg =
  match prg with
  | Id(ident) -> ident
  | _ -> failwith("get id: not an id");;
