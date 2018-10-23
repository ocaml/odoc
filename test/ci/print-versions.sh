#!/bin/bash

if [[ $ESY_BUILD == YES ]]; then
  case $TRAVIS_OS_NAME in
  "windows")
    # this is brittle
    esy --version
  ;;
  *)
    $(npm bin --global)/esy --version
  ;;
  esac
else
  opam --version
  ocaml -version
fi
