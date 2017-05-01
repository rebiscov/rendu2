open Prog;;
  
let new_queue () =
  ref ([], []);;

let rev_list l =
  let rec rev_aux l r =
    match l with
    | a::l1 -> rev_aux l1 (a::r)
    | [] -> r in
  rev_aux l [];;
  
let rec pop s =
  match !s with
  | ([], []) -> failwith("You're trying to pop and empty list !")
  | (l, a::r) -> s := (l,r); a
  | (l, []) -> s := ([], rev_list l); pop s;;

let push s a =
  let  (b, e) = !s in
  s := (a::b, e);;
    

let empty s =
  !s = ([], []);;

let is_fun prg =
  match prg with
  | Fun(_, _) | Recfun(_, _) -> true
  | _ -> false;;
