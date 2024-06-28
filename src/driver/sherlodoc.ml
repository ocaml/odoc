open Bos
open Cmd_outputs

let sherlodoc = Cmd.v "sherlodoc"


(* All paths relative to the html output dir *)

(* Per-package sherlodoc db for javascript search*)
let db_js_file pkg_dir =
  Fpath.(pkg_dir / "sherlodoc_db.js")

(* Global sherlodoc marshal file for server-side global search *)
let db_marshal_file =
  Fpath.v "sherlodoc_db.marshal"

(* Global static sherlodoc support file for javascript search *)
let js_file =
  Fpath.v "sherlodoc.js"

let index ?(ignore_output = false) ~format ~inputs ~dst ?favored_prefixes () =
  let desc = Printf.sprintf "Sherlodoc indexing at %s" (Fpath.to_string dst) in
  let format =
    Cmd.(v "--format" % match format with `marshal -> "marshal" | `js -> "js")
  in
  let favored_prefixes =
    match favored_prefixes with
    | None -> Cmd.empty
    | Some favored_prefixes ->
        Cmd.(v "--favoured_prefixes" % String.concat "," favored_prefixes)
  in
  let inputs = Cmd.(inputs |> List.map p |> of_list) in
  let cmd =
    Cmd.(
      sherlodoc % "index" %% format %% favored_prefixes %% inputs % "-o" % p dst)
  in
  let lines = submit desc cmd (Some dst) in
  if not ignore_output then
    add_prefixed_output cmd link_output (Fpath.to_string dst) lines

let js dst =
  let cmd = Cmd.(sherlodoc % "js" % p dst) in
  let desc = Printf.sprintf "Sherlodoc js at %s" (Fpath.to_string dst) in
  let _lines = submit desc cmd (Some dst) in
  ()
