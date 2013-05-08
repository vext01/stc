all: stc

stc: stc.ml ops.ml
	ocamlfind ocamlc -g -package num -linkpkg -o $@ ops.ml stc.ml
