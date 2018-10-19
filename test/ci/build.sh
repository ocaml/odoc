#!/bin/bash

if [[ $ESY_BUILD == YES ]]; then
	esy --verbose install
	esy --verbose build
	esy make test
else
  opam pin add -y --no-action odoc .
  opam install -y --deps-only odoc
  make test
  opam pin add -y --dev-repo dune
  make dune-test
fi
