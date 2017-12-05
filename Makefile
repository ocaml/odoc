.PHONY : build
build :
	jbuilder build --no-buffer --dev

.PHONY : test
test :
	jbuilder runtest --no-buffer --dev

BISECT_FILES_PATTERN := _build/default/test/*/bisect*.out

.PHONY : coverage
coverage :
	BISECT_ENABLE=yes jbuilder build --no-buffer --dev test/parser/test.exe
	(cd _build/default/test/parser && ./test.exe)
	bisect-ppx-report \
	    -I _build/default/ -html _coverage/ \
	    -text - -summary-only \
	    $(BISECT_FILES_PATTERN)
	@echo See _coverage/index.html

.PHONY : clean
clean :
	jbuilder clean
