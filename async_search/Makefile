.PHONY: build run

build : dune search.ml
	dune build search.ml

run : build
	dune exec -- ./search.exe $(ARGS) &

test : 
	echo "This is an echo server " | nc 127.0.0.1 8765

clean :
	killall echo.exe