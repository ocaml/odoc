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

let parent directories parent_cli_spec =
  let ap = Env.Accessible_paths.create ~directories in
  let find_parent :
      Odoc_model.Paths.Reference.t ->
      (Odoc_model.Lang.Page.t, [> `Msg of string ]) Result.result =
   fun r ->
    match r with
    | `Root (p, `TPage) | `Root (p, `TUnknown) -> (
        match Env.lookup_page ap p with
        | Some r -> Ok r
        | None -> Error (`Msg "Couldn't find specified parent page"))
    | _ -> Error (`Msg "Expecting page as parent")
  in
  let extract_parent = function
    | (`RootPage _ | `Page _) as container -> Ok container
    | _ -> Error (`Msg "Specified parent is not a parent of this file")
  in
  match parent_cli_spec with
  | CliParent f ->
      Odoc_model.Semantics.parse_reference f >>= fun r ->
      find_parent r >>= fun page ->
      extract_parent page.name >>= fun parent ->
      Ok (Explicit (parent, page.children))
  | CliPackage package -> Ok (Package (`RootPage (PageName.make_std package)))
  | CliNoparent -> Ok Noparent

let resolve_and_substitute ~env ~output ~warn_error parent input_file read_file
    =
  let filename = Fs.File.to_string input_file in

  read_file ~parent ~filename
  |> Odoc_model.Error.handle_errors_and_warnings ~warn_error
  >>= fun unit ->
  if not unit.Odoc_model.Lang.Compilation_unit.interface then
    Printf.eprintf "WARNING: not processing the \"interface\" file.%s\n%!"
      (if not (Filename.check_suffix filename "cmt") then "" (* ? *)
      else
        Printf.sprintf " Using %S while you should use the .cmti file" filename);
  let env = Env.build_from_module env unit in

  Odoc_xref2.Compile.compile env unit
  |> Odoc_xref2.Lookup_failures.handle_failures ~warn_error:false ~filename
  >>= fun compiled ->
  (* [expand unit] fetches [unit] from [env] to get the expansion of local, previously
     defined, elements. We'd rather it got back the resolved bit so we rebuild an
     environment with the resolved unit.
     Note that this is bad and once rewritten expand should not fetch the unit it is
     working on. *)
  (*    let expand_env = Env.build env (`Unit resolved) in*)
  (*    let expanded = Odoc_xref2.Expand.expand (Env.expander expand_env) resolved in *)
  Compilation_unit.save output compiled;
  Ok ()

let root_of_compilation_unit ~parent_spec ~hidden ~output ~module_name ~digest =
  let open Odoc_model.Root in
  let filename =
    Filename.chop_extension Fs.File.(to_string @@ basename output)
  in
  let result parent =
    let file_representation : Odoc_file.t =
      Odoc_file.create_unit ~force_hidden:hidden module_name
    in
    Ok
      {
        id = `Root (parent, ModuleName.make_std module_name);
        file = file_representation;
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
  | Noparent -> Error (`Msg "Compilation units require a parent")
  | Explicit (parent, children) ->
      if List.exists check_child children then result parent
      else Error (`Msg "Specified parent is not a parent of this file")
  | Package parent -> result parent

let mld ~parent_spec ~output ~children ~warn_error input =
  List.fold_left
    (fun acc child_str ->
      match (acc, Odoc_model.Semantics.parse_reference child_str) with
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
  let name =
    let check parents_children v =
      if List.exists check_child parents_children then Ok v
      else Error (`Msg "Specified parent is not a parent of this file")
    in
    match (parent_spec, children) with
    | Explicit (p, cs), [] -> check cs @@ `LeafPage (p, page_name)
    | Explicit (p, cs), _ -> check cs @@ `Page (p, page_name)
    | Package parent, [] -> Ok (`LeafPage (parent, page_name))
    | Package parent, _ ->
        Ok (`Page (parent, page_name)) (* This is a bit odd *)
    | Noparent, _ -> Ok (`RootPage page_name)
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
    Page.save output page;
    Ok ()
  in
  Fs.File.read input >>= fun str ->
  Odoc_loader.read_string
    (name :> Odoc_model.Paths.Identifier.LabelParent.t)
    input_s str
  |> Odoc_model.Error.handle_errors_and_warnings ~warn_error
  >>= function
  | `Stop -> resolve [] (* TODO: Error? *)
  | `Docs content -> resolve content

let compile ~env ~directories ~parent_cli_spec ~hidden ~children ~output
    ~warn_error input =
  parent directories parent_cli_spec >>= fun parent_spec ->
  let ext = Fs.File.get_ext input in
  if ext = ".mld" then mld ~parent_spec ~output ~warn_error ~children input
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
      | Noparent -> Error (`Msg "Compilation unit requires a parent")
      | Explicit (parent, _) -> Ok parent
      | Package parent -> Ok parent
    in
    parent >>= fun parent ->
    let make_root = root_of_compilation_unit ~parent_spec ~hidden ~output in
    resolve_and_substitute ~env ~output ~warn_error parent input
      (loader ~make_root)
