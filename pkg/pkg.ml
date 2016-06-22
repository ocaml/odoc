#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg



let opams =
  let lint_deps_excluding = Some ["menhir"; "menhirLib"] in
  [ Pkg.opam_file ~lint_deps_excluding "opam" ]

let () =
  Pkg.describe "doc-ock-xml" ~opams @@ fun c ->
  Ok [ Pkg.mllib "src/doc-ock-xml.mllib";
       Pkg.test ~run:false ~auto:false ~exts:(Exts.exts [".cmi"; ".cmo"])
         "test/coverage";
       Pkg.test
         ~args:Cmd.(v "-s" % "_build/test/coverage.cmi") "test/testCmi";
       Pkg.test
         ~args:Cmd.(v "-s" % "_build/test/coverage.cmti") "test/testCmti";
       Pkg.test
         ~args:Cmd.(v "-s" % "_build/test/coverage.cmt") "test/testCmt"; ]
