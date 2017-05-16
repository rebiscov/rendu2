let y= ref 1 in

let f x =
let y =ref 0 in raise (E 2) in
try f 1 with
 E x -> prInt !y;

y:=0;

let f x =
y:=1 ; raise  (E 2); y:=2 in
try f 1 with
 E x -> prInt !y;
y:=0;

 try y:=1 ; raise (E 2); y:=2 with
 E x -> prInt !y;
y:=1;

 try let y=ref 0 in raise (E 2) with
 E x -> prInt !y
 ;; 
