all: evaluation miniml

evaluation:
	ocamlbuild -use-ocamlfind evaluation.byte

miniml:
	ocamlbuild -use-ocamlfind miniml.byte

repl:
	./miniml.byte

