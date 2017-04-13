all:
	ocamlbuild -yaccflag -v -lib unix main.native
	mv main.native fouine

vincent:
	ocamlopt prog.ml
	ocamlc interpreter.mli
	ocamlopt interpreter.ml



clean:
	rm -R _build
	rm main.native

clean_v:
	rm *.cm*
	rm *.o
	rm a.out

