.PHONY: default
default: install build

.PHONY: build
build:
	@dune build

.PHONY: install
install:
	@opam update
	@opam install . --deps-only
	
.PHONY: test
test:
	@dune runtest

.PHONY: fmt
fmt:
	@dune fmt

.PHONY: clean
clean:
	@dune clean
