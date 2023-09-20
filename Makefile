build:
	dune build

utop:
	dune utop lib

run_exn_cost:
	dune exec -- ./bench_exn/exn_cost.exe -ascii -quota 1 -clear-columns time cycles 

run_exn_cost_off_backtrace:
	OCAMLRUNPARAM=b=0 dune exec -- ./bench_exn/exn_cost.exe -ascii -quota 1 -clear-columns time cycles

.PHONY: clean test

test:
	dune exec test/main.exe

clean:
	dune clean

