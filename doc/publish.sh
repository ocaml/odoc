#!/usr/bin/env bash

set -e


dune build @doc
dune build @docgen

ocaml-mdx test doc/driver.md

git checkout origin/gh-pages

rsync -av _build/default/_doc/ .

