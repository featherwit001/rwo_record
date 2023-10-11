.PHONY: build run clean

build : dune md5.ml
	@dune build

version : build
	@dune exec -- ./md5.exe -version

buildinfo : build
	@dune exec -- ./md5.exe -build-info
	
help : build
	@dune exec -- ./md5.exe -help

# make run ARGS='5 md5.ml'
run : build
	@dune exec -- ./md5.exe $(ARGS)