open Odoc_model
open Or_error
open Odoc_model.Names
module Id = Paths.Identifier

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
  | Explicit of Id.ContainerPage.t * Id.OdocId.t list
  | Package of Id.ContainerPage.t
  | Noparent

type parent_cli_spec =
  | CliParent of string
  | CliPackage of string
  | CliNoparent

(** Used to disambiguate child references. *)
let is_module_name n = String.length n > 0 && Astring.Char.Ascii.is_upper n.[0]

(** Accepted child references:

    - [page-foo] child is a container page.
    - [leafpage-foo] child is a leaf page.
    - [module-Foo] child is a module.
    - [Foo] for backward compatibility. *)
let parse_parent_child_reference s =
  let open Astring in
  match String.cut ~sep:"-" s with
  | Some ("page", n) -> Ok (`ContainerPage n)
  | Some ("leafpage", n) -> Ok (`LeafPage n)
  | Some ("module", n) when not (is_module_name n) ->
      Error (`Msg ("Not a module name: " ^ n))
  | Some ("module", n) -> Ok (`Module n)
  | Some (k, _) -> Error (`Msg ("Unrecognized kind: " ^ k))
  | None -> if is_module_name s then Ok (`Module s) else Ok (`UnqualifiedPage s)

(** Parse the child reference and return the identifier of the children. *)
let parse_child_reference id s =
  parse_parent_child_reference s >>= function
  | `ContainerPage n -> Ok (Id.Mk.page (Some id, PageName.make_std n))
  | `LeafPage n -> Ok (Id.Mk.leaf_page (Some id, PageName.make_std n))
  | `UnqualifiedPage _ ->
      Error
        (`Msg "Children reference must specify either 'page-' or 'leafpage-'.")
  | `Module n -> Ok (Id.Mk.root (Some id, ModuleName.make_std n))

(** Parse the parent reference and return the page name of the parent. *)
let parse_parent_reference s =
  parse_parent_child_reference s >>= function
  | `ContainerPage n | `UnqualifiedPage n -> Ok n
  | `LeafPage _ -> Error (`Msg "Parent cannot be a leaf page.")
  | `Module _ -> Error (`Msg "Expecting page as parent")

let parse_children_references id children =
  List.fold_left
    (fun acc child_str ->
      match (acc, parse_child_reference id child_str) with
      | Ok acc, Ok r -> Ok (r :: acc)
      | (Error _ as e), _ -> e
      | _, (Error _ as e) -> e)
    (Ok []) children

let parent resolver parent_cli_spec =
  let find_parent n =
    match Resolver.lookup_page resolver n with
    | Some r -> Ok r
    | None -> Error (`Msg "Couldn't find specified parent page")
  in
  let extract_parent = function
    | { Odoc_model.Paths.Identifier.iv = `Page _; _ } as container ->
        Ok container
    | _ -> Error (`Msg "Specified parent is not a parent of this file")
  in
  match parent_cli_spec with
  | CliParent f ->
      parse_parent_reference f >>= fun n ->
      find_parent n >>= fun page ->
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
    ~(parent : Odoc_model.Paths.Identifier.ContainerPage.t option) ~siblings
    input_file read_file =
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
  let unit =
    { unit with siblings; imports = resolve_imports resolver unit.imports }
  in
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

let is_child_of_parent id = function
  | Noparent -> Ok ()
  | Package _ -> Ok ()
  | Explicit (_, siblings) ->
      if List.exists (Id.OdocId.equal id) siblings then Ok ()
      else Error (`Msg "Specified parent is not a parent of this file")

let root_of_compilation_unit ~parent_spec ~hidden ~module_name ~digest =
  let open Odoc_model.Root in
  let parent =
    match parent_spec with
    | Noparent -> None
    | Explicit (parent, _) -> Some parent
    | Package parent -> Some parent
  in
  let id = Id.Mk.root (parent, ModuleName.make_std module_name) in
  let file = Odoc_file.create_unit ~force_hidden:hidden module_name in
  is_child_of_parent id parent_spec >>= fun () -> Ok { id; file; digest }

let mld ~parent_spec ~output ~children ~warnings_options input =
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
  let _ =
    match (parent_spec, root_name) with
    | Explicit _, "index" ->
        Format.eprintf
          "Warning: Potential name clash - child page named 'index'\n%!"
    | _ -> ()
  in
  (if children = [] then
   (* No children, this is a leaf page. *)
   let id =
     match parent_spec with
     | Explicit (p, _) -> Id.Mk.leaf_page (Some p, page_name)
     | Package parent -> Id.Mk.leaf_page (Some parent, page_name)
     | Noparent -> Id.Mk.leaf_page (None, page_name)
   in
   Ok (id, [])
  else
    (* Has children, this is a container page. *)
    let id =
      match parent_spec with
      | Explicit (p, _) -> Id.Mk.page (Some p, page_name)
      | Package parent ->
          Id.Mk.page (Some parent, page_name) (* This is a bit odd *)
      | Noparent -> Id.Mk.page (None, page_name)
    in
    parse_children_references id children >>= fun children ->
    Ok ((id :> Id.Page.t), children))
  >>= fun (name, children) ->
  let root =
    let file = Odoc_model.Root.Odoc_file.create_page root_name in
    { Odoc_model.Root.id = (name :> Id.OdocId.t); file; digest }
  in
  is_child_of_parent (name :> Id.OdocId.t) parent_spec >>= fun () ->
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
    (match parent_spec with
    | Noparent -> Ok (None, [])
    | Explicit (parent, siblings) -> Ok (Some parent, siblings)
    | Package parent -> Ok (Some parent, []))
    >>= fun (parent, siblings) ->
    let make_root = root_of_compilation_unit ~parent_spec ~hidden in
    let result =
      Odoc_model.Error.catch_errors_and_warnings (fun () ->
          resolve_and_substitute ~resolver ~parent ~siblings input
            (loader ~make_root))
    in
    (* Extract warnings to write them into the output file *)
    let _, warnings = Odoc_model.Error.unpack_warnings result in
    Odoc_model.Error.handle_errors_and_warnings ~warnings_options result
    >>= fun unit ->
    Odoc_file.save_unit output ~warnings unit;
    Ok ()
