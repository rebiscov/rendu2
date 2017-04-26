let new_stack () =
  ref [];;

let pop s =
  match !s with
  | a::r -> s := r; a
  | [] -> failwith("You're trying to pop and empty list !");;

let push s a =
  s := a::!s;;

let empty s =
  !s = [];;
