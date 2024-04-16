all: evaluation miniml tests

evaluation:
	ocamlbuild -use-ocamlfind evaluation.byte

miniml:
	ocamlbuild -use-ocamlfind miniml.byte

tests:
	ocamlbuild -use-ocamlfind tests.byte

test:
	./tests.byte

repl:
	./miniml.byte

