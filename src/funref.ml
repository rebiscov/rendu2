let a = ref 5 in
let f x = !a + x in
print f 5;
a := 6;
f 5;;
