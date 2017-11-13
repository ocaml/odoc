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

open StdLabels

let get_package root = Root.Package.to_string (Root.package root)

let from_odoc ~env ~output:root_dir input =
  let root = Root.read input in
  match Root.file root with
  | Page page_name ->
    let page = Page.load input in
    let odoctree =
      let resolve_env = Env.build env (`Page page) in
      Doc_model.resolve_page (Env.resolver resolve_env) page
    in
    let pkg_name = get_package root in
    let pages = Doc_html.To_html_tree.page ~get_package odoctree in
    let pkg_dir = Fs.Directory.reach_from ~dir:root_dir pkg_name in
    Fs.Directory.mkdir_p pkg_dir;
    Doc_html.Html_tree.traverse pages ~f:(fun ~parents _pkg_name content ->
      assert (parents = []);
      let oc =
        let f = Fs.File.create ~directory:pkg_dir ~name:(page_name ^ ".html") in
        open_out (Fs.File.to_string f)
      in
      let fmt = Format.formatter_of_out_channel oc in
      Format.fprintf fmt "%a" (Tyxml.Html.pp ()) content;
      close_out oc
    )
  | Compilation_unit {hidden; _} ->
    if hidden then
      Printf.eprintf
        "odoc should not generate html but will for the time being...\n%!";
    let unit = Compilation_unit.load input in
    let unit = Doc_model.Lookup.lookup unit in
    let odoctree =
      (* See comment in compile for explanation regarding the env duplication. *)
      let resolve_env = Env.build env (`Unit unit) in
      let resolved = Doc_model.resolve (Env.resolver resolve_env) unit in
      let expand_env = Env.build env (`Unit resolved) in
      Doc_model.expand (Env.expander expand_env) resolved
      |> Doc_model.Lookup.lookup
      |> Doc_model.resolve (Env.resolver expand_env) (* Yes, again. *)
    in
    let pkg_dir =
      let pkg_name = get_package root in
      Fs.Directory.reach_from ~dir:root_dir pkg_name
    in
    let pages = Doc_html.To_html_tree.unit ~get_package odoctree in
    Doc_html.Html_tree.traverse pages ~f:(fun ~parents name content ->
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

(* Used only for [--index-for] which is deprecated and available only for
   backward compatibility. It should be removed whenever. *)
let from_mld ~env ~package ~output:root_dir input =
  let root_name =
    Filename.chop_extension (Fs.File.(to_string @@ basename input))
  in
  let digest = Digest.file (Fs.File.to_string input) in
  let root =
    let file = Root.Odoc_file.create_page root_name in
    Root.create ~package ~file ~digest
  in
  let name = Doc_model.Paths.Identifier.Page (root, root_name) in
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
  match Fs.File.read input with
  | Error (`Msg s) ->
    Printf.eprintf "ERROR: %s\n%!" s;
    exit 1
  | Ok str ->
    let content =
      match Doc_model.Attrs.read_string name location str with
      | Stop -> Doc_model.Types.Documentation.Ok { text = [] ; tags = [] } (* TODO: Error? *)
      | Documentation content -> content
    in
    (* This is a mess. *)
    let page = Doc_model.Types.Page.{ name; content; digest } in
    let page = Doc_model.Lookup.lookup_page page in
    let env = Env.build env (`Page page) in
    let resolved = Doc_model.resolve_page (Env.resolver env) page in
    let pages = Doc_html.To_html_tree.page ~get_package resolved in
    let pkg_name = get_package root in
    let pkg_dir = Fs.Directory.reach_from ~dir:root_dir pkg_name in
    Fs.Directory.mkdir_p pkg_dir;
    Doc_html.Html_tree.traverse pages ~f:(fun ~parents _pkg_name content ->
      assert (parents = []);
      let oc =
        let f = Fs.File.create ~directory:pkg_dir ~name:"index.html" in
        open_out (Fs.File.to_string f)
      in
      let fmt = Format.formatter_of_out_channel oc in
      Format.fprintf fmt "%a" (Tyxml.Html.pp ()) content;
      close_out oc
    )
