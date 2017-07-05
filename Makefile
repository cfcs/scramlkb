#!/usr/bin/make -f
# -*- makefile -*-

OCAMLC = ocamlc
OCAMLFIND = ocamlfind
OCAMLOPT = ocamlopt

OCAMLOPTFLAGS=-package inotify,unix,bytes,str

PROGRAMS = scramlkb

PKG_NAME = scramlkb

LDFLAGS = -Wl,-Bstatic

scramlkb: scramlkb.ml
	# Static link the binary as it needs to run in minimal initramfs
	${OCAMLFIND} ${OCAMLOPT} ${OCAMLOPTFLAGS} -linkpkg -cclib '-static -dynamic -lz' scramlkb.ml -o scramlkb

test: scramlkb
	./scramlkb c 1 c| grep --color -o '.*'

clean:
	rm *.o *.cmi *.cmx scramlkb 2>/dev/null || true

