#!/bin/bash

set -x

if [[ $ESY_BUILD == YES ]]; then
  $(npm bin --global)/esy --version
else
  opam --version
  ocaml -version
fi

tidy --version
