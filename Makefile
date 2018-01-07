.PHONY : build
build :
	jbuilder build --dev

.PHONY : test
test : unit-test jbuilder-test

.PHONY : unit-test
unit-test : build
	jbuilder build --dev @tester
	(cd _build/default/test/parser && ./test.exe)
	(cd _build/default/test/html && ./test.exe)

ODOC_RELATIVE_PATH := ../../_build/install/default/bin/

.PHONY : jbuilder-test
jbuilder-test : build
	(cd test/jbuilder && PATH=$(ODOC_RELATIVE_PATH):$$PATH jbuilder build @doc)

COVERAGE := _coverage
BISECT_FILES_PATTERN := _build/default/test/*/bisect*.out

.PHONY : coverage
coverage :
	BISECT_ENABLE=yes jbuilder build --dev @tester
	find . -name 'bisect*.out' | xargs rm -f
	(cd _build/default/test/parser && ./test.exe) || true
	(cd _build/default/test/html && ./test.exe) || true
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
