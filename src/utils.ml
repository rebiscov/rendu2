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


  
