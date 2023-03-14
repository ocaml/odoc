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
open Or_error

type unit_content = Lang.Compilation_unit.t * Odoc_loader.Lookup_def.t option

type content =
  | Page_content of Lang.Page.t
  | Source_tree_content of Lang.SourceTree.t
  | Unit_content of unit_content

type t = { content : content; warnings : Odoc_model.Error.t list }

(** Written at the top of the files. Checked when loading. *)
let magic = "odoc-%%VERSION%%"

(** Exceptions while saving are allowed to leak. *)
let save_unit file (root : Root.t) (t : t) =
  Fs.Directory.mkdir_p (Fs.File.dirname file);
  let oc = open_out_bin (Fs.File.to_string file) in
  output_string oc magic;
  Marshal.to_channel oc root [];
  Marshal.to_channel oc t [];
  close_out oc

let save_page file ~warnings page =
  let dir = Fs.File.dirname file in
  let base = Fs.File.(to_string @@ basename file) in
  let file =
    if Astring.String.is_prefix ~affix:"page-" base then file
    else Fs.File.create ~directory:dir ~name:("page-" ^ base)
  in
  save_unit file page.Lang.Page.root { content = Page_content page; warnings }

let save_source_tree file ~warnings src_page =
  let dir = Fs.File.dirname file in
  let base = Fs.File.(to_string @@ basename file) in
  let file =
    if Astring.String.is_prefix ~affix:"src-" base then file
    else Fs.File.create ~directory:dir ~name:("src-" ^ base)
  in
  save_unit file src_page.Lang.SourceTree.root
    { content = Source_tree_content src_page; warnings }

let save_unit file ~warnings (m, s) =
  save_unit file m.Lang.Compilation_unit.root
    { content = Unit_content (m, s); warnings }

let load_ file f =
  let file = Fs.File.to_string file in
  let ic = open_in_bin file in
  let res =
    try
      let actual_magic = really_input_string ic (String.length magic) in
      if actual_magic = magic then
        let root = Marshal.from_channel ic in
        f ic root
      else
        let msg =
          Printf.sprintf "%s: invalid magic number %S, expected %S\n%!" file
            actual_magic magic
        in
        Error (`Msg msg)
    with exn ->
      let msg =
        Printf.sprintf "Error while unmarshalling %S: %s\n%!" file
          (match exn with Failure s -> s | _ -> Printexc.to_string exn)
      in
      Error (`Msg msg)
  in
  close_in ic;
  res

let load file = load_ file (fun ic _ -> Ok (Marshal.from_channel ic))

(** The root is saved separately in the files to support this function. *)
let load_root file = load_ file (fun _ root -> Ok root)
