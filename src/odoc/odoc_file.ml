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

open Odoc_utils
open ResultMonad
open Odoc_model

type unit_content = Lang.Compilation_unit.t

type content =
  | Page_content of Lang.Page.t
  | Impl_content of Lang.Implementation.t
  | Unit_content of unit_content
  | Asset_content of Lang.Asset.t

type t = { content : content; warnings : Odoc_model.Error.t list }

(** Written at the top of the files. Checked when loading. *)
let magic = "ODOC"

let magic_version = "%%VERSION%%"

(** Exceptions while saving are allowed to leak. *)
let save_ file f =
  let len = String.length magic_version in
  (* Sanity check, see similar check in load_ *)
  if len > 255 then
    failwith
      (Printf.sprintf
         "Magic version string %S is too long, must be <= 255 characters" magic);

  Fs.Directory.mkdir_p (Fs.File.dirname file);
  Io_utils.with_open_out_bin (Fs.File.to_string file) (fun oc ->
      output_string oc magic;
      output_binary_int oc len;
      output_string oc magic_version;
      f oc)

let save_unit file (root : Root.t) (t : t) =
  save_ file (fun oc ->
      Marshal.to_channel oc root [];
      Marshal.to_channel oc t [])

let save_page file ~warnings page =
  let dir = Fs.File.dirname file in
  let base = Fs.File.(to_string @@ basename file) in
  let file =
    if Astring.String.is_prefix ~affix:"page-" base then file
    else Fs.File.create ~directory:dir ~name:("page-" ^ base)
  in
  save_unit file page.Lang.Page.root { content = Page_content page; warnings }

let save_impl file ~warnings impl =
  let dir = Fs.File.dirname file in
  let base = Fs.File.(to_string @@ basename file) in
  let file =
    if Astring.String.is_prefix ~affix:"impl-" base then file
    else Fs.File.create ~directory:dir ~name:("impl-" ^ base)
  in
  save_unit file impl.Lang.Implementation.root
    { content = Impl_content impl; warnings }

let save_asset file ~warnings asset =
  let dir = Fs.File.dirname file in
  let base = Fs.File.(to_string @@ basename file) in
  let file =
    if Astring.String.is_prefix ~affix:"asset-" base then file
    else Fs.File.create ~directory:dir ~name:("asset-" ^ base)
  in
  let t = { content = Asset_content asset; warnings } in
  save_unit file asset.root t

let save_unit file ~warnings m =
  save_unit file m.Lang.Compilation_unit.root
    { content = Unit_content m; warnings }

let load_ file f =
  let file = Fs.File.to_string file in

  let check_exists () =
    if Sys.file_exists file then Ok ()
    else Error (`Msg (Printf.sprintf "File %s does not exist" file))
  in

  let check_magic ic =
    let actual_magic = really_input_string ic (String.length magic) in
    if actual_magic = magic then Ok ()
    else
      Error
        (`Msg
           (Printf.sprintf "%s has invalid magic %S, expected %S\n%!" file
              actual_magic magic))
  in
  let version_length ic () =
    let len = input_binary_int ic in
    if len > 0 && len <= 255 then Ok len
    else Error (`Msg (Printf.sprintf "%s has invalid version length" file))
  in
  let check_version ic len =
    let actual_magic = really_input_string ic len in
    if actual_magic = magic_version then Ok ()
    else
      let msg =
        Printf.sprintf "%s has invalid version %S, expected %S\n%!" file
          actual_magic magic_version
      in
      Error (`Msg msg)
  in

  check_exists () >>= fun () ->
  Io_utils.with_open_in_bin file @@ fun ic ->
  try
    check_magic ic >>= version_length ic >>= check_version ic >>= fun () -> f ic
  with exn ->
    let msg =
      Printf.sprintf "Error while unmarshalling %S: %s\n%!" file
        (match exn with Failure s -> s | _ -> Printexc.to_string exn)
    in
    Error (`Msg msg)

let load file =
  load_ file (fun ic ->
      let _root = Marshal.from_channel ic in
      Ok (Marshal.from_channel ic))

(** The root is saved separately in the files to support this function. *)
let load_root file =
  load_ file (fun ic ->
      let root = Marshal.from_channel ic in
      Ok root)

let save_index dst idx = save_ dst (fun oc -> Marshal.to_channel oc idx [])

let load_index file = load_ file (fun ic -> Ok (Marshal.from_channel ic))

let save_sidebar dst idx = save_ dst (fun oc -> Marshal.to_channel oc idx [])

let load_sidebar file = load_ file (fun ic -> Ok (Marshal.from_channel ic))
