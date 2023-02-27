open Or_error
open Odoc_model
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
  | Explicit of Paths.Identifier.ContainerPage.t * Paths.Reference.t list
  | Package of Paths.Identifier.ContainerPage.t
  | Noparent

type parent_cli_spec =
  | CliParent of string
  | CliPackage of string
  | CliNoparent

let check_is_none msg = function None -> Ok () | Some _ -> Error (`Msg msg)
let check_is_empty msg = function [] -> Ok () | _ :: _ -> Error (`Msg msg)

(** Raises warnings and errors. *)
let lookup_implementation_of_cmti intf_file =
  let input_file = Fs.File.set_ext ".cmt" intf_file in
  if Fs.File.exists input_file then
    let filename = Fs.File.to_string input_file in
    Odoc_loader.read_cmt_infos ~filename |> Error.raise_errors_and_warnings
  else (
    Error.raise_warning ~non_fatal:true
      (Error.filename_only
         "No implementation file found for the given interface"
         (Fs.File.to_string intf_file));
    None)

(** Parse parent and child references. May raise warnings. *)
let parse_reference f =
  (* This is a command-line error. *)
  let warnings_options = { Error.warn_error = true; print_warnings = true } in
  Semantics.parse_reference f
  |> Error.handle_errors_and_warnings ~warnings_options

let resolve_parent_page resolver f =
  let find_parent :
      Paths.Reference.t -> (Lang.Page.t, [> `Msg of string ]) Result.result =
   fun r ->
    match r with
    | `Root (p, `TPage) | `Root (p, `TUnknown) -> (
        match Resolver.lookup_page resolver p with
        | Some r -> Ok r
        | None -> Error (`Msg "Couldn't find specified parent page"))
    | _ -> Error (`Msg "Expecting page as parent")
  in
  let extract_parent = function
    | { Paths.Identifier.iv = `Page _; _ } as container -> Ok container
    | _ -> Error (`Msg "Specified parent is not a parent of this file")
  in
  parse_reference f >>= fun r ->
  find_parent r >>= fun page ->
  extract_parent page.name >>= fun parent -> Ok (parent, page.children)

let parent resolver parent_cli_spec =
  match parent_cli_spec with
  | CliParent f ->
      resolve_parent_page resolver f >>= fun (parent, children) ->
      Ok (Explicit (parent, children))
  | CliPackage package ->
      Ok (Package (Paths.Identifier.Mk.page (None, PageName.make_std package)))
  | CliNoparent -> Ok Noparent

let resolve_imports resolver imports =
  List.map
    (function
      | Lang.Compilation_unit.Import.Resolved _ as resolved -> resolved
      | Unresolved (name, _) as unresolved -> (
          match Resolver.resolve_import resolver name with
          | Some root -> Resolved (root, Names.ModuleName.make_std name)
          | None -> unresolved))
    imports

(** Raises warnings and errors. *)
let resolve_and_substitute ~resolver ~make_root ~source
    (parent : Paths.Identifier.ContainerPage.t option) input_file input_type =
  let filename = Fs.File.to_string input_file in
  (* [impl_shape] is used to lookup locations in the implementation. It is
     useless if no source code is given on command line. *)
  let should_read_impl_shape = source <> None in
  let unit, cmt_infos =
    match input_type with
    | `Cmti ->
        let unit =
          Odoc_loader.read_cmti ~make_root ~parent ~filename
          |> Error.raise_errors_and_warnings
        and cmt_infos =
          if should_read_impl_shape then
            lookup_implementation_of_cmti input_file
          else None
        in
        (unit, cmt_infos)
    | `Cmt ->
        Odoc_loader.read_cmt ~make_root ~parent ~filename
        |> Error.raise_errors_and_warnings
    | `Cmi ->
        let unit =
          Odoc_loader.read_cmi ~make_root ~parent ~filename
          |> Error.raise_errors_and_warnings
        in
        (unit, None)
  in
  let impl_shape =
    match cmt_infos with Some (shape, _) -> Some shape | None -> None
  in
  let source_info =
    match source with
    | Some id ->
        let infos =
          match cmt_infos with
          | Some (_, local_jmp) ->
              Odoc_loader.Source_info.of_local_jmp local_jmp
          | _ -> []
        in
        Some { Lang.Source_info.id; infos }
    | None -> None
  in
  if not unit.Lang.Compilation_unit.interface then
    Printf.eprintf "WARNING: not processing the \"interface\" file.%s\n%!"
      (if not (Filename.check_suffix filename "cmt") then "" (* ? *)
      else
        Printf.sprintf " Using %S while you should use the .cmti file" filename);
  (* Resolve imports, used by the [link-deps] command. *)
  let unit =
    { unit with imports = resolve_imports resolver unit.imports; source_info }
  in
  let env = Resolver.build_compile_env_for_unit resolver impl_shape unit in
  let compiled =
    Odoc_xref2.Compile.compile ~filename env unit |> Error.raise_warnings
  in
  (* [expand unit] fetches [unit] from [env] to get the expansion of local, previously
     defined, elements. We'd rather it got back the resolved bit so we rebuild an
     environment with the resolved unit.
     Note that this is bad and once rewritten expand should not fetch the unit it is
     working on. *)
  (*    let expand_env = Env.build env (`Unit resolved) in*)
  (*    let expanded = Odoc_xref2.Expand.expand (Env.expander expand_env) resolved in *)
  (compiled, impl_shape)

let root_of_compilation_unit ~parent_spec ~hidden ~output ~module_name ~digest =
  let open Root in
  let filename =
    Filename.chop_extension Fs.File.(to_string @@ basename output)
  in
  let result parent =
    let file = Odoc_file.create_unit ~force_hidden:hidden module_name in
    Ok
      {
        id = Paths.Identifier.Mk.root (parent, ModuleName.make_std module_name);
        file;
        digest;
      }
  in
  let check_child : Paths.Reference.t -> bool =
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

let page_name_of_output ~is_parent_explicit output =
  let page_dash_root =
    Filename.chop_extension Fs.File.(to_string @@ basename output)
  in
  let root_name =
    String.sub page_dash_root (String.length "page-")
      (String.length page_dash_root - String.length "page-")
  in
  (if is_parent_explicit then
   match root_name with
   | "index" ->
       Format.eprintf
         "Warning: Potential name clash - child page named 'index'\n%!"
   | _ -> ());
  root_name

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
    let is_parent_explicit =
      match parent_spec with Explicit _ -> true | _ -> false
    in
    page_name_of_output ~is_parent_explicit output
  in
  let input_s = Fs.File.to_string input in
  let digest = Digest.file input_s in
  let page_name = PageName.make_std root_name in
  let check_child : Paths.Reference.t -> bool =
   fun c ->
    match c with
    | `Root (n, `TUnknown) | `Root (n, `TPage) -> root_name = n
    | _ -> false
  in
  (if children = [] then
   (* No children, this is a leaf page. *)
   match parent_spec with
   | Explicit (p, _) -> Ok (Paths.Identifier.Mk.leaf_page (Some p, page_name))
   | Package parent ->
       Ok (Paths.Identifier.Mk.leaf_page (Some parent, page_name))
   | Noparent -> Ok (Paths.Identifier.Mk.leaf_page (None, page_name))
  else
    (* Has children, this is a container page. *)
    let check parents_children v =
      if List.exists check_child parents_children then Ok v
      else Error (`Msg "Specified parent is not a parent of this file")
    in
    (match parent_spec with
    | Explicit (p, cs) ->
        check cs @@ Paths.Identifier.Mk.page (Some p, page_name)
    | Package parent ->
        Ok (Paths.Identifier.Mk.page (Some parent, page_name))
        (* This is a bit odd *)
    | Noparent -> Ok (Paths.Identifier.Mk.page (None, page_name)))
    >>= fun id -> Ok (id :> Paths.Identifier.Page.t))
  >>= fun name ->
  let root =
    let file = Root.Odoc_file.create_page root_name in
    { Root.id = (name :> Paths.Identifier.OdocId.t); file; digest }
  in
  let resolve content =
    let page =
      Lang.Page.
        {
          name;
          root;
          children;
          source_children = [];
          content;
          digest;
          linked = false;
        }
    in
    Odoc_file.save_page output ~warnings:[] page;
    Ok ()
  in
  Fs.File.read input >>= fun str ->
  Odoc_loader.read_string (name :> Paths.Identifier.LabelParent.t) input_s str
  |> Error.handle_errors_and_warnings ~warnings_options
  >>= function
  | `Stop -> resolve [] (* TODO: Error? *)
  | `Docs content -> resolve content

let handle_file_ext = function
  | ".cmti" -> Ok `Cmti
  | ".cmt" -> Ok `Cmt
  | ".cmi" -> Ok `Cmi
  | _ ->
      Error (`Msg "Unknown extension, expected one of: cmti, cmt, cmi or mld.")

let compile ~resolver ~parent_cli_spec ~hidden ~children ~output
    ~warnings_options ~source input =
  parent resolver parent_cli_spec >>= fun parent_spec ->
  let ext = Fs.File.get_ext input in
  if ext = ".mld" then
    check_is_none "Not expecting source (--source) when compiling pages." source
    >>= fun () -> mld ~parent_spec ~output ~warnings_options ~children input
  else
    check_is_empty "Not expecting children (--child) when compiling modules."
      children
    >>= fun () ->
    (match source with
    | Some (parent, name) -> (
        Odoc_file.load parent >>= fun parent ->
        let err_not_parent () =
          Error (`Msg "Specified source-parent is not a parent of the source.")
        in
        match parent.Odoc_file.content with
        | Odoc_file.Page_content page -> (
            match page.Lang.Page.name with
            | { Paths.Identifier.iv = `Page _; _ } as parent_id ->
                let name = Paths.Identifier.Mk.source_page (parent_id, name) in
                if
                  List.exists
                    (Paths.Identifier.SourcePage.equal name)
                    page.source_children
                then Ok (Some name)
                else err_not_parent ()
            | { iv = `LeafPage _; _ } -> err_not_parent ())
        | Unit_content _ ->
            Error
              (`Msg "Specified source-parent should be a page but is a module.")
        )
    | None -> Ok None)
    >>= fun source ->
    handle_file_ext ext >>= fun input_type ->
    let parent =
      match parent_spec with
      | Noparent -> None
      | Explicit (parent, _) -> Some parent
      | Package parent -> Some parent
    in
    let make_root = root_of_compilation_unit ~parent_spec ~hidden ~output in
    let result =
      Error.catch_errors_and_warnings (fun () ->
          resolve_and_substitute ~resolver ~make_root ~source parent input
            input_type)
    in
    (* Extract warnings to write them into the output file *)
    let _, warnings = Error.unpack_warnings result in
    Error.handle_errors_and_warnings ~warnings_options result >>= fun unit ->
    Odoc_file.save_unit output ~warnings unit;
    Ok ()
