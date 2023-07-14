#!/bin/sh

ocamlc -c -bin-annot -dtypedtree test.ml
odoc compile page.mld --child module-test
odoc compile -I . --parent page test.cmt
odoc link -I . test.odoc
odoc html-generate --indent -o html test.odocl
odoc support-files -o html

