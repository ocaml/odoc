#!/bin/bash
ocamlc -c -bin-annot test.mli
odoc compile test.cmti
odoc link test.odoc
odoc html-generate test.odocl -o .
odoc support-files -o .

