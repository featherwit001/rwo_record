.PHONY: build run

build : dune echo.ml
	dune build echo.ml

run : build
	dune exec -- ./echo.exe $(ARGS) &

test : 
	echo "This is an echo server " | nc 127.0.0.1 8765

clean :
	killall echo.exe