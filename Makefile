init:
	opam sw create . 4.12.0 --no-install

install-deps:
	opam install . --deps-only

clean:
	dune clean

format:
	dune build @fmt --auto-promote

build:
	dune build

start:
	dune build --watch

test:
	dune exec src/tests/test.exe

hello_world:
	dune exec src/hello_world.exe
