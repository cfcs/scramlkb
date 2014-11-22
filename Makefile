scramlkb: scramlkb.ml
	ocamlbuild -lib unix scramlkb.native

test: scramlkb
	./scramlkb.native | cat
