build:
	dune build

clean:
	dune clean

install:
	dune build @install
	dune install

opam:
	opam install .

format:
	dune build @fmt --auto-promote
