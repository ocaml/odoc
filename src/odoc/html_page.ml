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

let to_html_tree_page ?theme_uri ~syntax v =
  match syntax with
  | Html.Tree.Reason -> Html.Generator.Reason.page ?theme_uri v
  | Html.Tree.OCaml -> Html.Generator.ML.page ?theme_uri v

let to_html_tree_compilation_unit ?theme_uri ~syntax v =
  match syntax with
  | Html.Tree.Reason -> Html.Generator.Reason.compilation_unit ?theme_uri v
  | Html.Tree.OCaml -> Html.Generator.ML.compilation_unit ?theme_uri v

let from_odoc ~env ?(syntax=Html.Tree.OCaml) ?theme_uri ~output:root_dir input =
  let root = Root.read input in
  match root.file with
  | Page page_name ->
    let page = Page.load input in
    let odoctree =
      let resolve_env = Env.build env (`Page page) in
      Xref.resolve_page (Env.resolver resolve_env) page
    in
    let pkg_name = root.package in
    let pages = to_html_tree_page ?theme_uri ~syntax odoctree in
    let pkg_dir = Fs.Directory.reach_from ~dir:root_dir pkg_name in
    Fs.Directory.mkdir_p pkg_dir;
    Html.Tree.traverse pages ~f:(fun ~parents _pkg_name content ->
      assert (parents = []);
      let oc =
        let f = Fs.File.create ~directory:pkg_dir ~name:(page_name ^ ".html") in
        open_out (Fs.File.to_string f)
      in
      let fmt = Format.formatter_of_out_channel oc in
      Format.fprintf fmt "%a@?" (Tyxml.Html.pp ()) content;
      close_out oc
    )
  | Compilation_unit {hidden = _; _} ->
    (* If hidden, we should not generate HTML. See
         https://github.com/ocaml/odoc/issues/99. *)
    let unit = Compilation_unit.load input in
    let unit = Xref.Lookup.lookup unit in
    let odoctree =
      (* See comment in compile for explanation regarding the env duplication. *)
      let resolve_env = Env.build env (`Unit unit) in
      let resolved = Xref.resolve (Env.resolver resolve_env) unit in
      let expand_env = Env.build env (`Unit resolved) in
      Xref.expand (Env.expander expand_env) resolved
      |> Xref.Lookup.lookup
      |> Xref.resolve (Env.resolver expand_env) (* Yes, again. *)
    in
    let pkg_dir =
      Fs.Directory.reach_from ~dir:root_dir root.package
    in
    let pages = to_html_tree_compilation_unit ?theme_uri ~syntax odoctree in
    Html.Tree.traverse pages ~f:(fun ~parents name content ->
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
      Format.fprintf fmt "%a@?" (Tyxml.Html.pp ()) content;
      close_out oc
    )

(* Used only for [--index-for] which is deprecated and available only for
   backward compatibility. It should be removed whenever. *)
let from_mld ~env ?(syntax=Html.Tree.OCaml) ~package ~output:root_dir input =
  let root_name =
    Filename.chop_extension (Fs.File.(to_string @@ basename input))
  in
  let digest = Digest.file (Fs.File.to_string input) in
  let root =
    let file = Model.Root.Odoc_file.create_page root_name in
    {Model.Root.package; file; digest}
  in
  let name = Model.Paths.Identifier.Page (root, root_name) in
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
      match Loader.read_string name location str with
      | Error e -> failwith (Model.Error.to_string e)
      | Ok (`Docs content) -> content
      | Ok `Stop -> [] (* TODO: Error? *)
    in
    (* This is a mess. *)
    let page = Model.Lang.Page.{ name; content; digest } in
    let page = Xref.Lookup.lookup_page page in
    let env = Env.build env (`Page page) in
    let resolved = Xref.resolve_page (Env.resolver env) page in
    let pages = to_html_tree_page ~syntax resolved in
    let pkg_dir = Fs.Directory.reach_from ~dir:root_dir root.package in
    Fs.Directory.mkdir_p pkg_dir;
    Html.Tree.traverse pages ~f:(fun ~parents _pkg_name content ->
      assert (parents = []);
      let oc =
        let f = Fs.File.create ~directory:pkg_dir ~name:"index.html" in
        open_out (Fs.File.to_string f)
      in
      let fmt = Format.formatter_of_out_channel oc in
      Format.fprintf fmt "%a@?" (Tyxml.Html.pp ()) content;
      close_out oc
    )
