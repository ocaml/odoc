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

open DocOckHtml

module Env = OdocEnv
module Unit = OdocUnit
module Root = OdocRoot
module Fs = OdocFs

let get_package root = Root.Package.to_string (Root.package root)

let unit ~env ~output:root_dir input =
  let unit = Unit.load input in
  let env = Env.build env unit in
  let odoctree =
    DocOck.resolve (Env.resolver env) unit
    |> DocOck.expand (Env.expander env)
  in
  let pkg_dir =
    let pkg_name = get_package (Unit.root unit) in
    Fs.Directory.reach_from ~dir:root_dir pkg_name
  in
  let pages = To_html_tree.unit ~get_package odoctree in
  Html_tree.traverse pages ~f:(fun ~parents name content ->
    let directory =
      let dir =
        List.fold_right ~f:(fun name dir -> Fs.Directory.reach_from ~dir name)
          parents ~init:pkg_dir
      in
      Fs.Directory.reach_from ~dir name
    in
    let oc =
      Fs.Directory.mkdir_p directory;
      let file = Fs.File.create ~directory ~name:"index.html" in
      open_out (Fs.File.to_string file)
    in
    let fmt = Format.formatter_of_out_channel oc in
    Format.fprintf fmt "%a" (Tyxml.Html.pp ()) content;
    close_out oc
  )

let from_mld ~env ~output:root_dir ~pkg input =
  let root_name = Fs.File.(to_string @@ basename input) in
  let root =
    let package = Root.Package.create pkg in
    let unit = Root.Unit.create root_name in
    let digest = Digest.file (Fs.File.to_string input) in
    Root.create ~package ~unit ~digest
  in
  let parent = DocOck.Paths.Identifier.Root (root, root_name) in
  let location =
    let pos =
      Lexing.{
        pos_fname = Fs.File.to_string input;
        pos_lnum = 0;
        pos_cnum = 0;
        pos_bol = 0
      }
    in
    Location.{ loc_start = pos; loc_end = pos; loc_ghost = true }
  in
  (* let ic = open_in (Fs.File.to_string input) in *)
  match Fs.File.read input with
  | Ok str ->
    let html =
      match DocOckAttrs.read_string parent location str with
      | Stop -> []
      | Documentation t ->
        let unit =
          DocOck.Types.Unit.{
            id = parent;
            doc = t;
            digest = Root.digest root;
            imports = []; (* Hm. *)
            source = None;
            interface = true;
            hidden = false;
            content = Module [];
            expansion = None;
          }
        in
        let env = Env.build env unit in
        let odoctree =
          DocOck.resolve (Env.resolver env) unit
          |> DocOck.expand (Env.expander env)
        in
        Html_tree.enter root_name;
        Documentation.to_html ~get_package odoctree.DocOck.Types.Unit.doc
    in
    begin match Html_tree.make (html, []) with
    | { Html_tree. content; children = []; _ } ->
      let directory = Fs.Directory.reach_from ~dir:root_dir pkg in
      let oc =
        Fs.Directory.mkdir_p directory;
        let file = Fs.File.create ~directory ~name:"index.html" in
        open_out (Fs.File.to_string file)
      in
      let fmt = Format.formatter_of_out_channel oc in
      Format.fprintf fmt "%a" (Tyxml.Html.pp ()) content;
      close_out oc
    | _ ->
      assert false
    end
  | Error (`Msg s) ->
    Printf.eprintf "ERROR: %s\n%!" s;
    exit 1
