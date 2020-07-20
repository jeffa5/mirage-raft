all:
	dune build @all @runtest

build:
	dune build

clean:
	dune clean
	make -C examples/unikernel clean

install:
	dune build @install
	dune install

opam:
	opam install .

format:
	dune build @fmt --auto-promote

.PHONY: test
test:
	dune exec test/main.exe

.PHONY: doc
doc:
	dune build @doc
