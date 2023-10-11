.PHONY: build bench clean

build: map_vs_hash.ml dune
	dune build

bench : build
	../_build/default/bench_map_vs_hash/map_vs_hash.exe -ascii -quota 1 -clear-columns time speedup

clean : 
	dune clean