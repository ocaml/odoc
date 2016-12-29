# This makefile is used for dev convenience. It is removed
# by the distribution process.

.PHONY: all clean

all:
	topkg build

test:
	topkg build
	topkg test

doc:
	topkg doc

clean:
	topkg clean
