init:
	opam sw create . 4.12.0 --no-install

install-deps:
	opam install . --deps-only

clean:
	dune clean

build-format:
	dune build @fmt --auto-promote

build:
	make build-format

start:
	dune exec src/hello_world.exe
