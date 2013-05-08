all: stc

stc: parse.ml types.ml stc.ml ops.ml
	ocamlfind ocamlc -g -package num -linkpkg -o $@ types.ml parse.ml ops.ml stc.ml

.PHONY: clean
clean:
	rm -f *.cmi *.cmo *.cmxa *.cmxo
