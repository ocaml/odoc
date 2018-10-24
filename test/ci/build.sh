#!/bin/bash

case $TRAVIS_OS_NAME in
"windows")
  # this is brittle
  ESY=/c/ProgramData/nvs/node/10.12.0/x64/node_modules/esy/_build/default/esy/bin/esyCommand.exe
;;
*)
  ESY=$(npm bin --global)/esy
;;
esac

if [[ $ESY_BUILD == YES ]]; then
  $ESY install --verbose
  $ESY build --verbose
  $ESY make test
else
  opam pin add -y --no-action odoc .
  opam install -y --deps-only odoc
  make test
  opam pin add -y --dev-repo dune
  make dune-test
fi
