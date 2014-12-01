scramlkb: scramlkb.ml
	ocamlfind ocamlopt -package unix,bytes -linkpkg scramlkb.ml -o scramlkb

test: scramlkb
	./scramlkb | cat

clean:
	rm *.o *.cmi *.cmx scramlkb 2>/dev/null || true

