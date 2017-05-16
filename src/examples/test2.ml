let rec f g n a =
  	if n = 0 then a 
  	else g (f g (n-1) a) in
  
let inc x = x + 1 in

f inc 5 0;;


