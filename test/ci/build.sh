#!/bin/bash

if [[ $ESY_BUILD == YES ]]; then
  make npm-build npm-test
else
  opam pin add -y --no-action odoc .
  opam install -y --deps-only odoc
  make test
  opam pin add -y --dev-repo dune
  make dune-test
  make docs
fi
