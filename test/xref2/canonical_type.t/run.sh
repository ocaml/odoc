#!/bin/sh
set -e
set -x

ocamlc -w -49 -bin-annot -no-alias-deps -c foo__.ml
ocamlc -bin-annot -open Foo__ -c foo__type0.ml
ocamlc -bin-annot -open Foo__ -c foo__type.ml
ocamlc -bin-annot -open Foo__ -c foo.ml
odoc compile --package x -I . foo__.cmt
odoc compile --package x -I . foo__type0.cmt
odoc compile --package x -I . foo__type.cmt
odoc compile --package x -I . foo.cmt
odoc link -I . foo.odoc
odoc html-generate foo.odocl -o html
odoc support-files -o html
