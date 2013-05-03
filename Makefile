all: stc

stc: stc.ml
	ocamlfind ocamlopt -package num -linkpkg -o stc stc.ml
