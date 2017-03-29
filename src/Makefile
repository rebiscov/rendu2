all:
	ocamlbuild -yaccflag -v -lib unix main.native

vincent:
	ocamlopt prog.ml
	ocamlc interpreter.mli
	ocamlopt interpreter.ml




clean:
	rm *.cm*
	rm *.o
	rm a.out
