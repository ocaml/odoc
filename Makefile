.PHONY : build
build :
	jbuilder build --dev

.PHONY : test
test : build
	jbuilder build @test/parser/runtest --dev --no-buffer -j 1
	jbuilder build @test/html/runtest --dev --no-buffer -j 1

ODOC_RELATIVE_PATH := ../../_build/install/default/bin/

.PHONY : jbuilder-test
jbuilder-test : build
	(cd test/jbuilder && PATH=$(ODOC_RELATIVE_PATH):$$PATH jbuilder build @doc)

COVERAGE := _coverage
BISECT_FILES_PATTERN := _build/default/test/*/bisect*.out

.PHONY : coverage
coverage :
	find . -name 'bisect*.out' | xargs rm -f
	BISECT_ENABLE=yes \
	  jbuilder build @test/parser/runtest --dev --no-buffer -j 1 --force
	BISECT_ENABLE=yes \
	  jbuilder build @test/html/runtest --dev --no-buffer -j 1 --force
	@bisect-ppx-report \
	    -I _build/default/ -html $(COVERAGE)/ \
	    -text - -summary-only \
	    $(BISECT_FILES_PATTERN)
	@echo See $(COVERAGE)/index.html

.PHONY : clean
clean :
	jbuilder clean
	(cd test/jbuilder && jbuilder clean)
	rm -rf $(COVERAGE)
