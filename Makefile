build:
	dune build

utop:
	dune utop src

.PHONY: test
test:
	dune exec test/main.exe

clean:
	dune clean

cloc:
	dune clean
	cloc --by-file --include-lang=OCaml .