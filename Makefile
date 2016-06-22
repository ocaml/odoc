# This makefile is used for dev convenience. It is removed
# by the distribution process.

.PHONY: lib test doc clean

lib:
	topkg build

test:
	topkg build
	topkg test

doc:
	topkg doc

clean:
	topkg clean
