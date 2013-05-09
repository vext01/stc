all: stc

SOURCES=	types.ml parse.ml ops.ml stc.ml

stc: parse.ml types.ml stc.ml ops.ml
	ocamlfind ocamlc -package num -linkpkg -o $@ ${SOURCES}

.PHONY: stc.opt
stc.opt: ${SOURCES}
	ocamlfind ocamlopt -package num -linkpkg -o $@ ${SOURCES}

.PHONY: stc.static
stc.static: ${SOURCES}
	ocamlfind ocamlopt -ccopt -Wl,-static -package num -linkpkg -o $@ ${SOURCES}

.PHONY: clean
clean: ${SOURCES}
	rm -f *.cmi *.cmo *.cmx *.cmxa *.cmxo *.o stc stc.opt stc.static
