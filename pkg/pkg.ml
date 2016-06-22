#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "doc-ock-html" @@ fun c ->
  Ok [ Pkg.mllib "src/doc-ock-html.mllib" ]
