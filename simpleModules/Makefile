.PHONY: all clean byte test repl utop

OCB_FLAGS = -tag bin_annot -use-menhir -use-ocamlfind -pkgs oUnit
OCB = ocamlbuild $(OCB_FLAGS)

all: byte

clean:
	$(OCB) -clean

byte:
	$(OCB) main.byte

repl: byte
	./repl.byte

utop: byte
	utop

test:
	$(OCB) checker_test.byte && ./checker_test.byte
