#!/bin/sh

ocamlc -c -bin-annot test.ml
odoc compile page.mld --child module-Test
odoc compile -I . --parent page test.cmt
odoc link -I . test.odoc
odoc html-generate --indent -o html test.odocl
odoc support-files -o html

