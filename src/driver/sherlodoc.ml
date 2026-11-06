open Bos

let sherlodoc = Cmd.v "sherlodoc"

(* All paths relative to the html output dir *)

(* Per-package sherlodoc db for javascript search*)
let db_js_file pkg_dir = Fpath.(pkg_dir / "sherlodoc_db.js")

(* Global static sherlodoc support file for javascript search *)
let js_file = Fpath.v "sherlodoc.js"

let index ~inputs ~dst =
  let desc = Printf.sprintf "Sherlodoc indexing at %s" (Fpath.to_string dst) in
  let inputs = Cmd.(inputs |> List.map p |> of_list) in
  let cmd =
    Cmd.(sherlodoc % "index" % "--format" % "js" %% inputs % "-o" % p dst)
  in
  Cmd_outputs.v
    ~log:(`Sherlodoc, Fpath.to_string dst)
    ~output:dst ~ignore_failures:true ~desc cmd

let js dst =
  let cmd = Cmd.(sherlodoc % "js" % p dst) in
  let desc = Printf.sprintf "Sherlodoc js at %s" (Fpath.to_string dst) in
  Cmd_outputs.v
    ~log:(`Sherlodoc, Fpath.to_string dst)
    ~output:dst ~ignore_failures:true ~desc cmd
