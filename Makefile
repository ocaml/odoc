# This makefile is used for dev convenience. It is removed
# by the distribution process.

.PHONY: all clean

all:
	topkg build

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
	topkg clean
	-@$(MAKE) -C test clean
