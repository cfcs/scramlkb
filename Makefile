scramlkb: scramlkb.ml
	ocamlfind ocamlopt -package unix,bytes -linkpkg scramlkb.ml -o scramlkb

test: scramlkb
	./scramlkb | cat
