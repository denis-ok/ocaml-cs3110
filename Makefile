init:
	opam sw create . 4.12.0 --no-install

install-deps:
	opam install . --deps-only

build:
	dune build

start:
	dune exec src/hello_world.exe
