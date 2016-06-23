#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "odoc" @@ fun c ->
  Ok [ Pkg.mllib "lib/odoc.mllib";
       Pkg.bin "bin/main" ~dst:"odoc" ]
