# This makefile is used for dev convenience. It is removed
# by the distribution process.

.PHONY: all clean

all:
	jbuilder build

doc:
	@echo "waiting for jbuilder support (cf. pull #74)"
	@# jbuilder odoc


clean:
	rm -R _build
