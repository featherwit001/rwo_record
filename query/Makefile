.PHONY: build run clean

SRC =  loader_handler.ml query_handler_core.ml query_handler.ml

build: dune $(SRC)
	dune build

run : build
	dune exec ./loader_handler.exe

clean : 
	dune clean

