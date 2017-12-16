.PHONY : build
build :
	jbuilder build --no-buffer --dev

.PHONY : test
test :
	jbuilder runtest --no-buffer --dev

COVERAGE := _coverage
BISECT_FILES_PATTERN := _build/default/test/*/bisect*.out

.PHONY : coverage
coverage :
	BISECT_ENABLE=yes jbuilder build --no-buffer --dev test/parser/test.exe
	(cd _build/default/test/parser && ./test.exe) || true
	@bisect-ppx-report \
	    -I _build/default/ -html $(COVERAGE)/ \
	    -text - -summary-only \
	    $(BISECT_FILES_PATTERN)
	@echo See $(COVERAGE)/index.html

.PHONY : clean
clean :
	jbuilder clean
	rm -rf $(COVERAGE)
