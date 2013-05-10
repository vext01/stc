all: stc

stc: parser.ml lexer.ml
	true


	ocamlc -c types.ml
	ocamlyacc -v parser.mly
	ocamlc -c parser.mli
	ocamllex lexer.mll
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c main.ml
	ocamlfind ocamlc -package num  -linkpkg -o test types.cmo lexer.cmo parser.cmo main.cmo

clean:
	rm -f *.cmi *.cmo lexer.ml parser.ml *.mli
