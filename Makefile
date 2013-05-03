all: stc

stc: stc.ml
	ocamlfind ocamlc -g -package num -linkpkg -o stc stc.ml
