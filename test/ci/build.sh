#!/bin/bash

set -x
set -e

ESY=$(npm bin --global)/esy

if [[ $ESY_BUILD == YES ]]; then
  $ESY install --verbose
  $ESY build --verbose
  $ESY make test
else
  opam pin add -y --no-action odoc .
  opam install -y --deps-only odoc
  make test
  make docs
fi
