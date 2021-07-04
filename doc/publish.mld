
#!/usr/bin/env bash

set -e


dune build @doc
dune build @docgen

cd ../_build/default/doc

ocaml-mdx test driver.md

cd ../../..

git checkout origin/gh-pages

rsync -av _build/default/doc/html/odoc/ .
