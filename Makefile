all:
	$(MAKE) -C lib
	$(MAKE) -C bin

test: all
	$(MAKE) -C test

silent_test: all
	$(MAKE) -C test compile 2> /dev/null
	$(MAKE) -C test odoc.html
	echo "================================================================="
	$(MAKE) -C test odocLink.html

coverage: all
	$(MAKE) -C test coverage

clean:
	-@$(MAKE) -C lib clean
	-@$(MAKE) -C bin clean
	-@$(MAKE) -C test clean

.PHONY: all clean
