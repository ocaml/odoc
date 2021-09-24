#!/usr/bin/env sh

OCAMLC=ocamlc
ODOC=odoc

$OCAMLC -w -49 -no-alias-deps -c odoc_bug__.ml -bin-annot

for f in a_intf b_intf; do
  $OCAMLC -c $f.ml -bin-annot -g -no-alias-deps -open Odoc_bug__ -o odoc_bug__$f
done

for f in b c; do
  $OCAMLC -c -intf $f.mli -bin-annot -no-alias-deps -open Odoc_bug__ -o odoc_bug__$f
done

for f in .cmt a_intf.cmt b_intf.cmt b.cmti c.cmti; do
  odoc compile odoc_bug__$f -I . --pkg odoc_bug
done

