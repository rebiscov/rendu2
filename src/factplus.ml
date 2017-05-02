let rec f n = 
	try
	if n < 0 then
		raise E 1
	else 
	if n = 0 then 
		1
	else
		n * (f (n-1))
	with E 1 -> (-1)
in
print (f 6) ;
print (f (-1));
print (f (-42))
;;
