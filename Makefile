scramlkb: scramlkb.ml
	ocamlfind ocamlopt -package inotify,unix,bytes,str -linkpkg scramlkb.ml -o scramlkb

test: scramlkb
	./scramlkb c 1 c| grep --color -o '.*'

clean:
	rm *.o *.cmi *.cmx scramlkb 2>/dev/null || true

