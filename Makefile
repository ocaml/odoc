.PHONY : build
build :
	dune build

.PHONY : test
test : build
	dune build @test/parser/runtest --no-buffer -j 1
	dune build @test/html/runtest --no-buffer -j 1

ODOC_RELATIVE_PATH := ../../_build/install/default/bin/

.PHONY : dune-test
dune-test : build
	(cd test/dune && PATH=$(ODOC_RELATIVE_PATH):$$PATH dune build @doc)

COVERAGE := _coverage
BISECT_FILES_PATTERN := _build/default/test/*/bisect*.out

.PHONY : coverage
coverage :
	find . -name 'bisect*.out' | xargs rm -f
	BISECT_ENABLE=yes dune build @test/parser/runtest --no-buffer -j 1 --force
	BISECT_ENABLE=yes dune build @test/html/runtest --no-buffer -j 1 --force
	@bisect-ppx-report \
	    -I _build/default/ -html $(COVERAGE)/ \
	    -text - -summary-only \
	    $(BISECT_FILES_PATTERN)
	@echo See $(COVERAGE)/index.html

.PHONY : clean
clean :
	dune clean
	(cd test/dune && dune clean)
	rm -rf $(COVERAGE)
