build:
	dune build

utop:
	dune utop lib

.PHONY: clean test

test:
	dune exec test/main.exe

clean:
	dune clean

