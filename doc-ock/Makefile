# This makefile is used for dev convenience. It is removed
# by the distribution process.

.PHONY: lib test doc clean

lib:
	jbuilder build -p doc-ock

test:
	jbuilder runtest
	topkg test

doc:
	@echo "waiting for jbuilder support (cf. pull #74)"
	@# jbuilder odoc

clean:
	rm -R _build
