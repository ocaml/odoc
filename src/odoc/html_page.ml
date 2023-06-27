(*
 * Copyright (c) 2014 Leo White <leo@lpw25.net>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Odoc_model

type args = {
  html_config : Odoc_html.Config.t;
  source_file : Fpath.t option;
  assets : Fpath.t list;
}

let render { html_config; source_file = _; assets = _ } page =
  Odoc_html.Generator.render ~config:html_config page

let source_documents source_info source_file ~syntax =
  match (source_info, source_file) with
  | Some { Lang.Source_info.id; infos }, Some src -> (
      match Fs.File.read src with
      | Error (`Msg msg) ->
          Error.raise_warning
            (Error.filename_only "Couldn't load source file: %s" msg
               (Fs.File.to_string src));
          []
      | Ok source_code ->
          let infos = infos @ Odoc_loader.Source_info.of_source source_code in
          [
            Odoc_document.Renderer.document_of_source ~syntax id infos
              source_code;
          ])
  | Some { id; _ }, None ->
      let filename = Paths.Identifier.name id in
      Error.raise_warning
        (Error.filename_only
           "The --source should be passed when generating documents from \
            compilation units that were compiled with --source-parent and \
            --source-name"
           filename);
      []
  | None, Some src ->
      Error.raise_warning
        (Error.filename_only
           "--source argument is invalid on compilation unit that were not \
            compiled with --source-parent and --source-name"
           (Fs.File.to_string src));
      []
  | None, None -> []

let asset_documents parent_id children asset_paths =
  let asset_names =
    List.filter_map
      (function Lang.Page.Asset_child name -> Some name | _ -> None)
      children
  in
  let rec extract paths name =
    match paths with
    | [] -> (paths, (name, None))
    | x :: xs when Fpath.basename x = name -> (xs, (name, Some x))
    | x :: xs ->
        let rest, elt = extract xs name in
        (x :: rest, elt)
  in
  let unmatched, paired_or_missing =
    List.fold_left_map extract asset_paths asset_names
  in
  List.iter
    (fun asset ->
      Error.raise_warning
        (Error.filename_only "this asset was not declared as a child of %s"
           (Paths.Identifier.name parent_id)
           (Fs.File.to_string asset)))
    unmatched;
  List.filter_map
    (fun (name, path) ->
      match path with
      | None ->
          Error.raise_warning (Error.filename_only "asset is missing." name);
          None
      | Some path ->
          let asset_id = Paths.Identifier.Mk.asset_file (parent_id, name) in
          let url = Odoc_document.Url.Path.from_identifier asset_id in
          Some (Odoc_document.Types.Document.Asset { url; src = path }))
    paired_or_missing

let extra_documents args input ~syntax =
  match input with
  | Odoc_document.Renderer.CU unit ->
      source_documents unit.Lang.Compilation_unit.source_info args.source_file
        ~syntax
  | Page page -> asset_documents page.Lang.Page.name page.children args.assets

let renderer = { Odoc_document.Renderer.name = "html"; render; extra_documents }
