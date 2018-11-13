#!/bin/bash

if [[ $ESY_BUILD == YES ]]; then
  case $TRAVIS_OS_NAME in
  "windows")
    # this is brittle
    /c/ProgramData/nvs/node/10.13.0/x64/node_modules/esy/_build/default/esy/bin/esyCommand.exe --version
  ;;
  *)
    $(npm bin --global)/esy --version
  ;;
  esac
else
  opam --version
  ocaml -version
fi
