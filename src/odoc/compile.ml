open Odoc_model
open Odoc_model.Names
open Or_error
open Odoc_utils

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

type package_spec = { package : string; output : Fpath.t }
type parent_spec = {
  parent : string option;
  children : string list;
  output : Fpath.t;
}

type parent_id_spec = { parent_id : string; output_dir : string }

type cli_spec =
  | CliNoParent of Fpath.t
  | CliPackage of package_spec
  | CliParent of parent_spec
  | CliParentId of parent_id_spec

type spec = {
  parent_id : Paths.Identifier.ContainerPage.t option;
  output : Fpath.t;
  parents_children : Lang.Page.child list option;
  children : string list;
}

let rec path_of_id output_dir id =
  match id with
  | None -> Fpath.v output_dir
  | Some id -> (
      match (id : Paths.Identifier.ContainerPage.t).iv with
      | `Page (parent, p) ->
          let d = path_of_id output_dir parent in
          Fpath.(d / PageName.to_string p))

let check_is_empty msg = function [] -> Ok () | _ :: _ -> Error (`Msg msg)

(** Used to disambiguate child references. *)
let is_module_name n = String.length n > 0 && Char.Ascii.is_upper n.[0]

(** Accepted child references:

    - [asset-foo] child is an arbitrary asset
    - [module-Foo] child is a module.
    - [module-foo], [Foo] child is a module, for backward compatibility.
    - [page-foo] child is a container or leaf page.
    - [srctree-foo] child is a source tree

    Parses [...-"foo"] as [...-foo] for backward compatibility. *)
let parse_parent_child_reference s =
  let unquote s =
    let len = String.length s in
    if String.head s = Some '"' && String.head ~rev:true s = Some '"' && len > 1
    then String.with_range ~first:1 ~len:(len - 2) s
    else s
  in
  match String.cut ~sep:"-" s with
  | Some ("page", n) -> Ok (Lang.Page.Page_child (unquote n))
  | Some ("module", n) ->
      Ok (Module_child (unquote (String.Ascii.capitalize n)))
  | Some ("src", _) -> Error (`Msg "Implementation unexpected")
  | Some (k, _) -> Error (`Msg ("Unrecognized kind: " ^ k))
  | None -> if is_module_name s then Ok (Module_child s) else Ok (Page_child s)

let resolve_parent_page resolver f =
  let find_parent = function
    | Lang.Page.Page_child p -> (
        match Resolver.lookup_page resolver p with
        | Some r -> Ok r
        | None -> Error (`Msg "Couldn't find specified parent page"))
    | Module_child _ -> Error (`Msg "Expecting page as parent")
  in
  let extract_parent = function
    | { Paths.Identifier.iv = `Page _; _ } as container -> Ok container
    | { Paths.Identifier.iv = `LeafPage _; _ } ->
        Error (`Msg "Specified parent is not a parent of this file")
  in
  parse_parent_child_reference f >>= fun r ->
  find_parent r >>= fun page ->
  extract_parent page.name >>= fun parent -> Ok (parent, page.children)

let mk_id str =
  match str with
  | "" -> None
  | str -> (
      let l = String.cuts ~sep:"/" str in
      List.fold_left
        (fun acc id ->
          Some (Paths.Identifier.Mk.page (acc, PageName.make_std id)))
        None l
      |> function
      | Some x -> Some x
      | None -> failwith "Failed to create ID")

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
let resolve_and_substitute ~resolver ~make_root ~hidden ~warnings_tag
    (parent : Paths.Identifier.ContainerPage.t option) input_file input_type =
  let filename = Fs.File.to_string input_file in
  let unit =
    match input_type with
    | `Cmti ->
        Odoc_loader.read_cmti ~make_root ~parent ~filename ~warnings_tag
        |> Error.raise_errors_and_warnings
    | `Cmt ->
        Odoc_loader.read_cmt ~make_root ~parent ~filename ~warnings_tag
        |> Error.raise_errors_and_warnings
    | `Cmi ->
        Odoc_loader.read_cmi ~make_root ~parent ~filename ~warnings_tag
        |> Error.raise_errors_and_warnings
  in
  let unit = { unit with hidden = hidden || unit.hidden } in
  if not unit.Lang.Compilation_unit.interface then
    Printf.eprintf "WARNING: not processing the \"interface\" file.%s\n%!"
      (if not (Filename.check_suffix filename "cmt") then "" (* ? *)
       else
         Printf.sprintf " Using %S while you should use the .cmti file" filename);
  (* Resolve imports, used by the [link-deps] command. *)
  let unit = { unit with imports = resolve_imports resolver unit.imports } in
  let env = Resolver.build_compile_env_for_unit resolver unit in
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
  compiled

let root_of_compilation_unit ~parent_id ~parents_children ~hidden ~output
    ~module_name ~digest =
  let open Root in
  let result parent =
    let file = Odoc_file.create_unit ~force_hidden:hidden module_name in
    Ok
      {
        id = Paths.Identifier.Mk.root (parent, ModuleName.make_std module_name);
        file;
        digest;
      }
  in
  let check_child = function
    | Lang.Page.Module_child n ->
        let filename =
          Filename.chop_extension Fs.File.(to_string @@ basename output)
        in
        String.Ascii.(uncapitalize n = uncapitalize filename)
    | Page_child _ -> false
  in
  match parents_children with
  | Some parents_children ->
      if List.exists check_child parents_children then result parent_id
      else Error (`Msg "Specified parent is not a parent of this file")
  | None -> result parent_id

(*
     let d = path_of_id parent in
     Fs.Directory.mkdir_p (Fs.Directory.of_string (Fpath.to_string d));
     let file = Odoc_file.create_unit ~force_hidden:hidden module_name in
     Ok {
       id = Paths.Identifier.Mk.root (Some parent, ModuleName.make_std module_name);
       file;
       digest;
     }
   in *)

let name_of_output ~prefix output =
  let page_dash_root =
    Filename.chop_extension Fs.File.(to_string @@ basename output)
  in
  String.drop ~max:(String.length prefix) page_dash_root

let page_name_of_output output = name_of_output ~prefix:"page-" output

let is_index_page = function
  | { Paths.Identifier.iv = `Page _; _ } -> false
  | { iv = `LeafPage (_, p); _ } ->
      Astring.String.equal (Names.PageName.to_string p) "index"

let has_children_order { Frontmatter.children_order; _ } =
  Option.is_some children_order

let mld ~parent_id ~parents_children ~output ~children ~warnings_options
    ~short_title input =
  List.fold_left
    (fun acc child_str ->
      match (acc, parse_parent_child_reference child_str) with
      | Ok acc, Ok r -> Ok (r :: acc)
      | Error m, _ -> Error m
      | _, Error (`Msg m) ->
          Error (`Msg ("Failed to parse child reference: " ^ m))
      | _, Error _ -> Error (`Msg "Unknown failure parsing child reference"))
    (Ok []) children
  >>= fun children ->
  let root_name = page_name_of_output output in
  let input_s = Fs.File.to_string input in
  let digest = Digest.file input_s in
  let page_name = PageName.make_std root_name in
  let check_child = function
    | Lang.Page.Page_child n -> root_name = n
    | Module_child _ -> false
  in
  (if children = [] then
     (* No children, this is a leaf page. *)
     Ok (Paths.Identifier.Mk.leaf_page (parent_id, page_name))
   else
     (* Has children, this is a container page. *)
     let check parents_children =
       if List.exists check_child parents_children then Ok ()
       else Error (`Msg "Specified parent is not a parent of this file")
     in
     (match parents_children with
     | Some parents_children ->
         check parents_children >>= fun () ->
         Ok (Paths.Identifier.Mk.page (parent_id, page_name))
     | None -> Ok (Paths.Identifier.Mk.page (parent_id, page_name)))
     >>= fun id -> Ok (id :> Paths.Identifier.Page.t))
  >>= fun id ->
  let resolve content frontmatter =
    let zero_heading = Comment.find_zero_heading content.Comment.elements in
    if (not (is_index_page id)) && has_children_order frontmatter then
      Error.raise_warning
        (Error.filename_only "Non-index page cannot specify @children_order."
           input_s);
    let root =
      let file =
        Root.Odoc_file.create_page root_name zero_heading frontmatter
      in
      { Root.id = (id :> Paths.Identifier.OdocId.t); file; digest }
    in
    let rec conv :
        Comment.inline_element -> Comment.non_link_inline_element option =
      function
      | #Comment.leaf_inline_element as e -> Some e
      | `Styled (s, es) ->
          Some
            (`Styled
               ( s,
                 List.filter_map
                   (function
                     | { Location_.location; value } -> (
                         match conv value with
                         | Some value -> Some { Location_.location; value }
                         | None -> None))
                   es ))
      | `Reference _ | `Link _ -> None
    in
    let frontmatter =
      match short_title with
      | None -> frontmatter
      | Some t -> (
          let t =
            Odoc_parser.parse_comment ~location:Lexing.dummy_pos ~text:t
          in
          let v = Odoc_parser.ast t in
          let parent_of_sections =
            Odoc_model.Paths.Identifier.Mk.page
              (None, Odoc_model.Names.PageName.make_std "None")
          in
          let v =
            Odoc_model.Semantics.ast_to_comment ~internal_tags:Expect_none
              ~tags_allowed:false ~parent_of_sections v []
          in
          let v, _ = Error.raise_warnings v in
          match v with
          | { location = _; value = `Paragraph ({ value = e; location } :: _) }
            :: _ -> (
              match conv e with
              | None -> frontmatter
              | Some e ->
                  {
                    frontmatter with
                    short_title = Some [ { value = e; location } ];
                  })
          | _ -> frontmatter)
    in
    let page =
      Lang.Page.
        {
          name = id;
          root;
          children;
          content;
          digest;
          linked = false;
          frontmatter;
        }
    in
    Odoc_file.save_page output ~warnings:[] page;
    ()
  in
  Fs.File.read input >>= fun str ->
  Error.handle_errors_and_warnings ~warnings_options
  @@ Error.catch_errors_and_warnings
  @@ fun () ->
  Odoc_loader.read_string (id :> Paths.Identifier.LabelParent.t) input_s str
  |> Error.raise_errors_and_warnings
  |> function
  | content, page_tags -> resolve content page_tags

let handle_file_ext ext =
  match ext with
  | ".cmti" -> Ok `Cmti
  | ".cmt" -> Ok `Cmt
  | ".cmi" -> Ok `Cmi
  | _ ->
      Error (`Msg "Unknown extension, expected one of: cmti, cmt, cmi or mld.")

let resolve_spec ~input resolver cli_spec =
  match cli_spec with
  | CliParent { parent; children; output } ->
      (let root_name = name_of_output ~prefix:"page-" output in
       match root_name with
       | "index" ->
           Format.eprintf
             "Warning: Potential name clash - child page named 'index'\n%!"
       | _ -> ());
      let parent =
        match parent with
        | Some parent -> (
            match resolve_parent_page resolver parent with
            | Ok (parent_id, parents_children) ->
                Ok (Some parent_id, Some parents_children)
            | Error e -> Error e)
        | None -> Ok (None, None)
      in
      parent >>= fun (parent_id, parents_children) ->
      Ok { parent_id; parents_children; children; output }
  | CliPackage { package; output } ->
      Ok
        {
          parent_id =
            Some (Paths.Identifier.Mk.page (None, PageName.make_std package));
          output;
          parents_children = None;
          children = [];
        }
  | CliParentId { parent_id; output_dir } ->
      let parent_id = mk_id parent_id in
      let directory =
        path_of_id output_dir parent_id
        |> Fpath.to_string |> Fs.Directory.of_string
      in
      let name =
        let ext = Fs.File.get_ext input in
        let name = Fs.File.set_ext ".odoc" input in
        let name = Fs.File.basename name in
        if ext = ".mld" then "page-" ^ Fs.File.to_string name
        else name |> Fpath.to_string |> String.Ascii.uncapitalize
      in
      let output = Fs.File.create ~directory ~name in
      Ok { parent_id; output; parents_children = None; children = [] }
  | CliNoParent output ->
      Ok { output; parent_id = None; parents_children = None; children = [] }

let compile ~resolver ~hidden ~cli_spec ~warnings_options ~short_title input =
  resolve_spec ~input resolver cli_spec
  >>= fun { parent_id; output; parents_children; children } ->
  let ext = Fs.File.get_ext input in
  if ext = ".mld" then
    mld ~parent_id ~parents_children ~output ~warnings_options ~children
      ~short_title input
  else
    check_is_empty "Not expecting children (--child) when compiling modules."
      children
    >>= fun () ->
    handle_file_ext ext >>= fun input_type ->
    let make_root =
      root_of_compilation_unit ~parent_id ~parents_children ~hidden ~output
    in
    let result =
      Error.catch_errors_and_warnings (fun () ->
          resolve_and_substitute ~resolver ~make_root ~hidden
            ~warnings_tag:warnings_options.warnings_tag parent_id input
            input_type)
    in
    (* Extract warnings to write them into the output file *)
    let _, warnings = Error.unpack_warnings result in
    Error.handle_errors_and_warnings ~warnings_options result >>= fun unit ->
    Odoc_file.save_unit output ~warnings unit;
    Ok ()
