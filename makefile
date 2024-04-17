all: evaluation miniml tests

evaluation:
	ocamlbuild -use-ocamlfind evaluation.byte

miniml:
	ocamlbuild -use-ocamlfind miniml.byte

tests:
	ocamlbuild -use-ocamlfind tests.byte

test:
	./tests.byte

utop:
	dune build --profile release
	dune utop . --profile release

repl:
	./miniml.byte

clean:
	rm -rf _build *.byte