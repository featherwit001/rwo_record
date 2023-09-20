build:
	dune build

run:
	dune exec -- ./exn_cost.exe -ascii -quota 1 -clear-columns time cycles 

run_off:
	OCAMLRUNPARAM=b=0 dune exec -- ./exn_cost.exe -ascii -quota 1 -clear-columns time cycles

.PHONY: clean test

clean:
	dune clean