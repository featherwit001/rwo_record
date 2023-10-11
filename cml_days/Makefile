.PHONY: build run clean

build : dune days.ml
	@dune build

version : build
	@dune exec -- ./days.exe -version

buildinfo : build
	@dune exec -- ./days.exe -build-info
	
help : build
	@dune exec -- ./days.exe -help

# make run ARGS='5 days.ml'
run : build
	@dune exec -- ./days.exe $(ARGS)