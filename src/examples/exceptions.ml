try 
if 2 = 2 then
	raise (E 2)
else
	raise (E 4)
with E x -> x
;;
