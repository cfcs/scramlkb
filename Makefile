scramlkb: scramlkb.ml
	ocamlfind ocamlopt -package unix,bytes -linkpkg scramlkb.ml -o scramlkb

systemd-askpass-scrambled: systemd-askpass-scrambled.ml
	ocamlfind ocamlopt -package lwt,inotify.lwt -linkpkg systemd-askpass-scrambled.ml -o systemd-askpass-scrambled

test: scramlkb
	./scramlkb c 1 c| grep --color -o '.*'

clean:
	rm *.o *.cmi *.cmx scramlkb 2>/dev/null || true

