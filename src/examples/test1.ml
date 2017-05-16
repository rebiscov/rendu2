let rec pow x n =
  if n = 0 then 1
  else x * (pow x (n-1)) in
print pow 3 3;
    
let r = ref 4 in
print (pow !r 2);

let rec ack m n =
  if m = 0 then n+1
  else if n = 0 then (ack (m-1) 1)
  else (ack (m-1) (ack m (n-1))) in

print (ack 3 4);

print (ack 3 3);

let rec f g n a =
  if n = 0 then a
  else (g (f g (n-1) a)) in

let inc x = x + 1 in

f inc 0 0;;
