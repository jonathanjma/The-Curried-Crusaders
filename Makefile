build:
	dune build

utop:
	dune utop src

ustove:
	OCAMLRUNPARAM=b dune exec bin/main.exe

.PHONY: test
test:
	dune exec test/main.exe

clean:
	rm -rf *.coverage _coverage
	dune clean

bisect:
	rm -f *.coverage
	-dune exec --instrument-with bisect_ppx test/main.exe
	bisect-ppx-report html

cloc:
	dune clean
	cloc --by-file --include-lang=OCaml .
	dune build

parse_explain:
	menhir src/parser.mly --explain