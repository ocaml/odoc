#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let etc_dir =
  let doc = "Use $(docv) as the etc install directory" in
  Conf.(key "etc-dir" fpath ~absent:"etc" ~doc)

let etc_config c =
  match Conf.build_context c with
  | `Dev -> Ok ()
  | `Pin | `Distrib ->
    let cfg = Printf.sprintf "let dir = %S" (Conf.value c etc_dir) in
    OS.File.write "bin/odoc_etc.ml" cfg

let () =
  let build = Pkg.build ~pre:etc_config () in
  Pkg.describe "odoc" ~build @@ fun c ->
  Ok [ Pkg.etc "etc/odoc.css";
       Pkg.mllib "lib/odoc.mllib";
       Pkg.bin "bin/main" ~dst:"odoc";
       Pkg.test "test/odoc_odoc"; ]
