#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "doc-ock" @@ fun c ->
  Ok [ Pkg.lib ~exts:(Exts.ext ".cmi") "src/docOckTypes";
       Pkg.mllib "src/doc-ock.mllib";
       Pkg.test ~run:false ~auto:false ~exts:(Exts.exts [".cmi"; ".cmo"])
         "test/coverage";
       Pkg.test ~args:(Cmd.v "_build/test/coverage.cmi") "test/testCmi";
       Pkg.test ~args:(Cmd.v "_build/test/coverage.cmti") "test/testCmti";
       Pkg.test ~args:(Cmd.v "_build/test/coverage.cmt") "test/testCmt"; ]
