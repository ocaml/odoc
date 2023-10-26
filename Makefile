.PHONY : build
build :
	dune build

.PHONY : publish-docs
publish-docs:
	dune build @doc
	dune build @docgen || true
	git checkout gh-pages
	rsync -av _build/default/doc/html/odoc/ .

.PHONY : test
test :
	dune runtest

# ODOC_BENCHMARK=true will cause the driver to build a larger set of libraries
# containing more edge cases.
.PHONY : bench
bench:
	@ODOC_BENCHMARK=true dune build @bench

.PHONY : coverage
coverage :
	dune build --instrument-with bisect_ppx @test/runtest --no-buffer -j 1 --force || true
	bisect-ppx-report html
	@echo See _coverage/index.html

.PHONY : clean
clean :
	dune clean
	rm -r _coverage
