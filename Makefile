all:
	omake

.PHONY: static
static:
	omake OCAMLOPTFLAGS="-ccopt -Wl,-static" stc.opt

.PHONY: clean
clean:
	omake clean
