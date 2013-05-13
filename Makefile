all:
	omake

.PHONY: static
static:
	omake OCAMLOPTFLAGS="-ccopt -Wl,-static -ccopt -static-libgcc" stc.opt

.PHONY: clean
clean:
	omake clean
