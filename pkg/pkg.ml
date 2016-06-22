#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "octavius" @@ fun c ->
  Ok [ Pkg.lib ~exts:(Exts.ext ".cmi") "src/octTypes";
       Pkg.mllib "src/octavius.mllib"; ]
