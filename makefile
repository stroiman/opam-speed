build:
	dune build -w

test:
	DUNE_BUILD_DIR=_test dune runtest -w

install:
	dune build
	dune install
	opam install .

.PHONY: install build test
