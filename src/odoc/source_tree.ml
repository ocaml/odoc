open Astring
open Odoc_model
open Odoc_model.Names
open Or_error
module Id = Paths.Identifier

let check_is_child_of_parent siblings root_name =
  let check_child = function
    | Lang.Page.Source_tree_child n -> root_name = n
    | Page_child _ | Asset_child _ | Module_child _ -> false
  in
  if List.exists check_child siblings then Ok ()
  else Error (`Msg "Specified parent is not a parent of this file")

(** Each path is represented as a list of segments. Order is not preserved. *)
let parse_input_file input =
  let parse_path p = Fs.File.of_string p |> Fs.File.segs in
  let is_sep = function '\n' | '\r' -> true | _ -> false in
  Fs.File.read input >>= fun content ->
  Ok
    ( Digest.file (Fpath.to_string input),
      String.fields ~empty:false ~is_sep content |> List.rev_map parse_path )

let source_child_id parent segs = Id.Mk.source_page (parent, segs)

let compile ~resolver ~parent ~output ~warnings_options:_ input =
  let root_name = Compile.name_of_output ~prefix:"src-" output in
  let page_name = PageName.make_std root_name in
  Compile.resolve_parent_page resolver parent >>= fun (parent, siblings) ->
  let id = Id.Mk.page (Some parent, page_name) in
  check_is_child_of_parent siblings root_name >>= fun () ->
  parse_input_file input >>= fun (digest, source_tree) ->
  let root =
    let file = Root.Odoc_file.create_page root_name in
    { Root.id = (id :> Id.OdocId.t); file; digest }
  in
  let source_children = List.rev_map (source_child_id id) source_tree in
  let page =
    Lang.SourceTree.{ name = (id :> Id.Page.t); root; source_children; digest }
  in
  Odoc_file.save_source_tree output ~warnings:[] page;
  Ok ()
