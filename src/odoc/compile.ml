open Or_error
open Odoc_model.Names

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

type parent_spec =
  | Explicit of
      Odoc_model.Paths.Identifier.ContainerPage.t
      * Odoc_model.Paths.Reference.t list
  | Package of Odoc_model.Paths.Identifier.ContainerPage.t
  | Noparent

type parent_cli_spec =
  | CliParent of string
  | CliPackage of string
  | CliNoparent

(** Parse parent and child references. May print warnings. *)
let parse_reference f =
  let open Odoc_model in
  (* This is a command-line error. *)
  let warnings_options = { Error.warn_error = true; print_warnings = true } in
  Semantics.parse_reference f
  |> Error.handle_errors_and_warnings ~warnings_options

let parent resolver parent_cli_spec =
  let find_parent :
      Odoc_model.Paths.Reference.t ->
      (Odoc_model.Lang.Page.t, [> `Msg of string ]) Result.result =
   fun r ->
    match r with
    | `Root (p, `TPage) | `Root (p, `TUnknown) -> (
        match Resolver.lookup_page resolver p with
        | Some r -> Ok r
        | None -> Error (`Msg "Couldn't find specified parent page"))
    | _ -> Error (`Msg "Expecting page as parent")
  in
  let extract_parent = function
    | { Odoc_model.Paths.Identifier.iv = `Page _; _ } as container ->
        Ok container
    | _ -> Error (`Msg "Specified parent is not a parent of this file")
  in
  match parent_cli_spec with
  | CliParent f ->
      parse_reference f >>= fun r ->
      find_parent r >>= fun page ->
      extract_parent page.name >>= fun parent ->
      Ok (Explicit (parent, page.children))
  | CliPackage package ->
      Ok
        (Package
           (Odoc_model.Paths.Identifier.Mk.page
              (None, PageName.make_std package)))
  | CliNoparent -> Ok Noparent

let resolve_imports resolver imports =
  let open Odoc_model in
  List.map
    (function
      | Lang.Compilation_unit.Import.Resolved _ as resolved -> resolved
      | Unresolved (name, _) as unresolved -> (
          match Resolver.resolve_import resolver name with
          | Some root -> Resolved (root, Names.ModuleName.make_std name)
          | None -> unresolved))
    imports

(** Raises warnings and errors. *)
let resolve_and_substitute ~resolver
    (parent : Odoc_model.Paths.Identifier.ContainerPage.t option) input_file
    read_file =
  let filename = Fs.File.to_string input_file in
  let unit =
    read_file ~parent ~filename |> Odoc_model.Error.raise_errors_and_warnings
  in
  if not unit.Odoc_model.Lang.Compilation_unit.interface then
    Printf.eprintf "WARNING: not processing the \"interface\" file.%s\n%!"
      (if not (Filename.check_suffix filename "cmt") then "" (* ? *)
      else
        Printf.sprintf " Using %S while you should use the .cmti file" filename);
  (* Resolve imports, used by the [link-deps] command. *)
  let unit = { unit with imports = resolve_imports resolver unit.imports } in
  let env = Resolver.build_env_for_unit resolver ~linking:false unit in
  let compiled =
    Odoc_xref2.Compile.compile ~filename env unit
    |> Odoc_model.Error.raise_warnings
  in
  (* [expand unit] fetches [unit] from [env] to get the expansion of local, previously
     defined, elements. We'd rather it got back the resolved bit so we rebuild an
     environment with the resolved unit.
     Note that this is bad and once rewritten expand should not fetch the unit it is
     working on. *)
  (*    let expand_env = Env.build env (`Unit resolved) in*)
  (*    let expanded = Odoc_xref2.Expand.expand (Env.expander expand_env) resolved in *)
  compiled

let root_of_compilation_unit ~parent_spec ~hidden ~output ~module_name ~digest =
  let open Odoc_model.Root in
  let filename =
    Filename.chop_extension Fs.File.(to_string @@ basename output)
  in
  let result parent =
    let file = Odoc_file.create_unit ~force_hidden:hidden module_name in
    Ok
      {
        id =
          Odoc_model.Paths.Identifier.Mk.root
            (parent, ModuleName.make_std module_name);
        file;
        digest;
      }
  in
  let check_child : Odoc_model.Paths.Reference.t -> bool =
   fun c ->
    match c with
    | `Root (n, `TUnknown) | `Root (n, `TModule) ->
        Astring.String.Ascii.(uncapitalize n = uncapitalize filename)
    | _ -> false
  in
  match parent_spec with
  | Noparent -> result None
  | Explicit (parent, children) ->
      if List.exists check_child children then result (Some parent)
      else Error (`Msg "Specified parent is not a parent of this file")
  | Package parent -> result (Some parent)

let mld ~parent_spec ~output ~children ~warnings_options input =
  List.fold_left
    (fun acc child_str ->
      match (acc, parse_reference child_str) with
      | Ok acc, Ok r -> Ok (r :: acc)
      | Error m, _ -> Error m
      | _, Error (`Msg m) ->
          Error (`Msg ("Failed to parse child reference: " ^ m))
      | _, Error _ -> Error (`Msg "Unknown failure parsing child reference"))
    (Ok []) children
  >>= fun children ->
  let root_name =
    let page_dash_root =
      Filename.chop_extension Fs.File.(to_string @@ basename output)
    in
    String.sub page_dash_root (String.length "page-")
      (String.length page_dash_root - String.length "page-")
  in
  let input_s = Fs.File.to_string input in
  let digest = Digest.file input_s in
  let page_name = PageName.make_std root_name in
  let check_child : Odoc_model.Paths.Reference.t -> bool =
   fun c ->
    match c with
    | `Root (n, `TUnknown) | `Root (n, `TPage) -> root_name = n
    | _ -> false
  in
  let _ =
    match (parent_spec, root_name) with
    | Explicit _, "index" ->
        Format.eprintf
          "Warning: Potential name clash - child page named 'index'\n%!"
    | _ -> ()
  in
  let name =
    let check parents_children v =
      if List.exists check_child parents_children then Ok v
      else Error (`Msg "Specified parent is not a parent of this file")
    in
    let module Mk = Odoc_model.Paths.Identifier.Mk in
    match (parent_spec, children) with
    | Explicit (p, cs), [] -> check cs @@ Mk.leaf_page (Some p, page_name)
    | Explicit (p, cs), _ -> check cs @@ Mk.page (Some p, page_name)
    | Package parent, [] -> Ok (Mk.leaf_page (Some parent, page_name))
    | Package parent, _ ->
        Ok (Mk.page (Some parent, page_name)) (* This is a bit odd *)
    | Noparent, [] -> Ok (Mk.leaf_page (None, page_name))
    | Noparent, _ -> Ok (Mk.page (None, page_name))
  in
  name >>= fun name ->
  let root =
    let file = Odoc_model.Root.Odoc_file.create_page root_name in
    {
      Odoc_model.Root.id = (name :> Odoc_model.Paths.Identifier.OdocId.t);
      file;
      digest;
    }
  in
  let resolve content =
    let page =
      Odoc_model.Lang.Page.
        { name; root; children; content; digest; linked = false }
    in
    Odoc_file.save_page output ~warnings:[] page;
    Ok ()
  in
  Fs.File.read input >>= fun str ->
  Odoc_loader.read_string
    (name :> Odoc_model.Paths.Identifier.LabelParent.t)
    input_s str
  |> Odoc_model.Error.handle_errors_and_warnings ~warnings_options
  >>= function
  | `Stop -> resolve [] (* TODO: Error? *)
  | `Docs content -> resolve content

let compile ~resolver ~parent_cli_spec ~hidden ~children ~output
    ~warnings_options input =
  parent resolver parent_cli_spec >>= fun parent_spec ->
  let ext = Fs.File.get_ext input in
  if ext = ".mld" then
    mld ~parent_spec ~output ~warnings_options ~children input
  else
    (match ext with
    | ".cmti" -> Ok Odoc_loader.read_cmti
    | ".cmt" -> Ok Odoc_loader.read_cmt
    | ".cmi" -> Ok Odoc_loader.read_cmi
    | _ ->
        Error
          (`Msg "Unknown extension, expected one of: cmti, cmt, cmi or mld."))
    >>= fun loader ->
    let parent =
      match parent_spec with
      | Noparent -> Ok None
      | Explicit (parent, _) -> Ok (Some parent)
      | Package parent -> Ok (Some parent)
    in
    parent >>= fun parent ->
    let make_root = root_of_compilation_unit ~parent_spec ~hidden ~output in
    let result =
      Odoc_model.Error.catch_errors_and_warnings (fun () ->
          resolve_and_substitute ~resolver parent input (loader ~make_root))
    in
    (* Extract warnings to write them into the output file *)
    let _, warnings = Odoc_model.Error.unpack_warnings result in
    Odoc_model.Error.handle_errors_and_warnings ~warnings_options result
    >>= fun unit ->
    Odoc_file.save_unit output ~warnings unit;
    Ok ()
