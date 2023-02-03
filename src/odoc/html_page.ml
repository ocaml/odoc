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

type args = { html_config : Odoc_html.Config.t; source_file : Fpath.t option }

let render { html_config; source_file = _ } page =
  Odoc_html.Generator.render ~config:html_config page

let extra_documents args unit ~syntax =
  match (unit.Lang.Compilation_unit.source_info, args.source_file) with
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
      let filename = Paths.Identifier.SourcePage.name id in
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

let renderer = { Odoc_document.Renderer.name = "html"; render; extra_documents }
