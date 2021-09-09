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

watch:
	dune build --watch

start:
	dune exec src/hello_world.exe

test:
	dune exec src/tests/test.exe
