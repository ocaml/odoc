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

(* We are slightly more flexible here than OCaml usually is, and allow
   'linking' of modules that have the same name. This is because we do
   documentation at a package level - it's perfectly acceptable to have
   libraries within a package that are never meant to be linked into the same
   binary, however package-level documents such as module and type indexes
   effectively have to link those libraries together. Hence we may find
   ourselves in the unfortunate situation where there are multiple modules with the same
   name in our include path. We therefore maintain a mapping of module/page
   name to Root _list_. Where we've already made a judgement about which module
   we're looking for we have a digest, and can pick the correct module. When we
   don't (for example, when handling package-level mld files), we pick the
   first right now. The ocamldoc syntax doesn't currently allow for specifying
   more accurately than just the module name anyway.

   Where we notice this ambiguity we warn the user to wrap their libraries,
   which will generally fix this issue. *)

open Or_error

module Named_roots : sig
  type t

  type error = NoPackage | NoRoot

  val create : (string * Fs.Directory.t) list -> current_root:string option -> t

  val all_of : ?root:string -> ext:string -> t -> (Fs.File.t list, error) result

  val find_by_path :
    ?root:string -> t -> path:Fs.File.t -> (Fs.File.t option, error) result

  val find_by_name :
    ?root:string -> t -> name:string -> (Fs.File.t list, error) result
end = struct
  type flat =
    | Unvisited of Fs.Directory.t
    | Visited of (string, Fs.File.t) Hashtbl.t

  type hierarchical = (Fs.File.t, Fs.File.t) Hashtbl.t * Fs.Directory.t

  type pkg = { flat : flat; hierarchical : hierarchical }

  type t = { table : (string, pkg) Hashtbl.t; current_root : string option }

  type error = NoPackage | NoRoot

  let hashtbl_find_opt cache package =
    match Hashtbl.find cache package with
    | x -> Some x
    | exception Not_found -> None

  let create pkglist ~current_root =
    let cache = Hashtbl.create 42 in
    List.iter
      (fun (pkgname, root) ->
        let flat = Unvisited root
        and hierarchical = (Hashtbl.create 42, root) in
        Hashtbl.add cache pkgname { flat; hierarchical })
      pkglist;
    { current_root; table = cache }

  let find_by_path ?root { table = cache; current_root } ~path =
    let path = Fpath.normalize path in
    let root =
      match (root, current_root) with
      | Some pkg, _ | None, Some pkg -> Ok pkg
      | None, None -> Error NoRoot
    in
    root >>= fun root ->
    match hashtbl_find_opt cache root with
    | Some { hierarchical = cache, root; _ } -> (
        match hashtbl_find_opt cache path with
        | Some x -> Ok (Some x)
        | None ->
            let full_path = Fpath.( // ) (Fs.Directory.to_fpath root) path in
            if Fs.File.exists full_path then (
              Hashtbl.add cache path full_path;
              Ok (Some full_path))
            else Ok None)
    | None -> Error NoPackage

  let populate_flat_namespace ~root =
    let flat_namespace = Hashtbl.create 42 in
    let () =
      match
        Fs.Directory.fold_files_rec_result
          (fun () path ->
            let name = Fpath.filename path in
            Ok (Hashtbl.add flat_namespace name path))
          () root
      with
      | Ok () -> ()
      | Error _ -> assert false
      (* The function passed to [fold_files_rec_result] never returns [Error _] *)
    in
    flat_namespace

  let find_by_name ?root { table = cache; current_root } ~name =
    let package =
      match (root, current_root) with
      | Some pkg, _ | None, Some pkg -> Ok pkg
      | None, None -> Error NoRoot
    in
    package >>= fun package ->
    match hashtbl_find_opt cache package with
    | Some { flat = Visited flat; _ } -> Ok (Hashtbl.find_all flat name)
    | Some ({ flat = Unvisited root; _ } as p) ->
        let flat = populate_flat_namespace ~root in
        Hashtbl.replace cache package { p with flat = Visited flat };
        Ok (Hashtbl.find_all flat name)
    | None -> Error NoPackage

  let all_of ?root ~ext { table; current_root } =
    (match (root, current_root) with
    | None, Some current_root -> Ok current_root
    | Some pkg, _ -> Ok pkg
    | None, None -> Error NoRoot)
    >>= fun my_root ->
    let return flat =
      let values = Hashtbl.fold (fun _ v acc -> v :: acc) flat [] in
      let values = List.filter (Fpath.has_ext ext) values in
      Ok values
    in
    match Hashtbl.find table my_root with
    | { flat = Visited flat; _ } -> return flat
    | { flat = Unvisited root; _ } as p ->
        let flat = populate_flat_namespace ~root in
        Hashtbl.replace table my_root { p with flat = Visited flat };
        return flat
    | exception Not_found -> Error NoPackage
end

let () = (ignore Named_roots.find_by_name [@warning "-5"])
let () = (ignore Named_roots.find_by_path [@warning "-5"])

module Accessible_paths : sig
  type t

  val create : directories:Fs.Directory.t list -> t

  val find : t -> string -> Fs.File.t list
end = struct
  type t = (string, Fpath.t (* list *)) Hashtbl.t

  let create ~directories =
    let unit_cache = Hashtbl.create 42 in
    List.iter
      (fun directory ->
        try
          let files = Sys.readdir (Fs.Directory.to_string directory) in
          Array.iter
            (fun file ->
              let file = Fpath.v file in
              if Fpath.has_ext "odoc" file then
                Hashtbl.add unit_cache
                  (Astring.String.Ascii.capitalize
                     (file |> Fpath.rem_ext |> Fpath.basename))
                  (Fs.File.append directory file))
            files
        with Sys_error _ ->
          (* TODO: Raise a warning if a directory given as -I cannot be opened *)
          ())
      directories;
    unit_cache

  let find t name =
    let name = Astring.String.Ascii.capitalize name in
    Hashtbl.find_all t name
end

module Hierarchy : sig
  (** Represent a file hierarchy and allow file path manipulations that do not
      escape it. *)

  type t

  type error = [ `Escape_hierarchy ]

  val make : current_package:Fs.Directory.t -> current_dir:Fs.Directory.t -> t

  val resolve_relative : t -> Fs.File.t -> (Fs.File.t, error) result
  (** [resolve_relative h relpath] resolve [relpath] relatively to the current
      directory, making sure not to escape the hierarchy. *)
end = struct
  type t = { current_package : Fs.Directory.t; current_dir : Fs.Directory.t }

  type error = [ `Escape_hierarchy ]

  let make ~current_package ~current_dir = { current_package; current_dir }

  let resolve_relative t relpath =
    let path = Fs.File.append t.current_dir relpath in
    if Fs.Directory.contains ~parentdir:t.current_package path then Ok path
    else Error `Escape_hierarchy
end

module StringMap = Map.Make (String)

let build_imports_map imports =
  List.fold_left
    (fun map import ->
      match import with
      | Odoc_model.Lang.Compilation_unit.Import.Unresolved (name, _) ->
          StringMap.add name import map
      | Odoc_model.Lang.Compilation_unit.Import.Resolved (_, name) ->
          StringMap.add (Odoc_model.Names.ModuleName.to_string name) import map)
    StringMap.empty imports

let root_name root = Odoc_model.Root.Odoc_file.name root.Odoc_model.Root.file

let unit_name
    ( Odoc_file.Unit_content { root; _ }
    | Page_content { root; _ }
    | Impl_content { root; _ }
    | Source_tree_content { root; _ } ) =
  root_name root

(** TODO: Propagate warnings instead of printing. *)
let load_unit_from_file path =
  match Odoc_file.load path with
  | Ok u -> Some u.content
  | Error (`Msg msg) ->
      let warning =
        Odoc_model.Error.filename_only "%s" msg (Fs.File.to_string path)
      in
      prerr_endline (Odoc_model.Error.to_string warning);
      None

let load_units_from_files paths =
  let safe_read file acc =
    match load_unit_from_file file with Some u -> u :: acc | None -> acc
  in
  List.fold_right safe_read paths []

let unit_cache = Hashtbl.create 42

(** Load every units matching a given name. Cached. *)
let load_units_from_name =
  let do_load ap target_name =
    let paths = Accessible_paths.find ap target_name in
    load_units_from_files paths
  in
  fun ap target_name ->
    try Hashtbl.find unit_cache target_name
    with Not_found ->
      let units = do_load ap target_name in
      Hashtbl.add unit_cache target_name units;
      units

let rec find_map f = function
  | [] -> None
  | hd :: tl -> (
      match f hd with Some x -> Some (x, tl) | None -> find_map f tl)

let lookup_unit_with_digest ap target_name digest =
  let unit_that_match_digest u =
    match u with
    | Odoc_file.Unit_content m
      when Digest.compare m.Odoc_model.Lang.Compilation_unit.digest digest = 0
      ->
        Some m
    | _ -> None
  in
  let units = load_units_from_name ap target_name in
  match find_map unit_that_match_digest units with
  | Some (m, _) -> Odoc_xref2.Env.Found m
  | None -> Not_found

(** Lookup a compilation unit matching a name. If there is more than one
    result, report on stderr and return the first one.

    TODO: Correctly propagate warnings instead of printing. *)
let lookup_unit_by_name ap target_name =
  let first_unit u =
    match u with
    | Odoc_file.Unit_content m -> Some m
    | Impl_content _ | Page_content _ | Source_tree_content _ -> None
  in
  let rec find_ambiguous tl =
    match find_map first_unit tl with
    | Some (m, tl) -> m :: find_ambiguous tl
    | None -> []
  in
  let units = load_units_from_name ap target_name in
  match find_map first_unit units with
  | Some (m, tl) ->
      (match find_ambiguous tl with
      | [] -> ()
      | ambiguous ->
          let ambiguous = m :: ambiguous in
          let ambiguous =
            List.map
              (fun m -> root_name m.Odoc_model.Lang.Compilation_unit.root)
              ambiguous
          in
          let warning =
            Odoc_model.Error.filename_only
              "Ambiguous lookup. Possible files: %a"
              Format.(pp_print_list pp_print_string)
              ambiguous target_name
          in
          prerr_endline (Odoc_model.Error.to_string warning));
      Some m
  | None -> None

(** Lookup an unit. First looks into [imports_map] then searches into the
    paths. *)
let lookup_unit ~important_digests ~imports_map ap target_name =
  let of_option f =
    match f with Some m -> Odoc_xref2.Env.Found m | None -> Not_found
  in
  match StringMap.find target_name imports_map with
  | Odoc_model.Lang.Compilation_unit.Import.Unresolved (_, Some digest) ->
      lookup_unit_with_digest ap target_name digest
  | Unresolved (_, None) ->
      if important_digests then Odoc_xref2.Env.Forward_reference
      else of_option (lookup_unit_by_name ap target_name)
  | Resolved (root, _) -> lookup_unit_with_digest ap target_name root.digest
  | exception Not_found ->
      if important_digests then Odoc_xref2.Env.Not_found
      else of_option (lookup_unit_by_name ap target_name)

(** Lookup a page.

    TODO: Warning on ambiguous lookup. *)
let lookup_page ap target_name =
  let target_name = "page-" ^ target_name in
  let is_page u =
    match u with
    | Odoc_file.Page_content p -> Some p
    | Impl_content _ | Unit_content _ | Source_tree_content _ -> None
  in
  let units = load_units_from_name ap target_name in
  match find_map is_page units with Some (p, _) -> Some p | None -> None

(** Lookup an implementation. *)
let lookup_impl ap target_name =
  let target_name = "impl-" ^ Astring.String.Ascii.uncapitalize target_name in
  let is_impl u =
    match u with
    | Odoc_file.Impl_content p -> Some p
    | Page_content _ | Unit_content _ | Source_tree_content _ -> None
  in
  let units = load_units_from_name ap target_name in
  match find_map is_impl units with Some (p, _) -> Some p | None -> None

(** Add the current unit to the cache. No need to load other units with the same
    name. *)
let add_unit_to_cache u =
  let target_name =
    (match u with
    | Odoc_file.Page_content _ -> "page-"
    | Impl_content _ -> "impl-"
    | Unit_content _ -> ""
    | Source_tree_content _ -> "page-")
    ^ unit_name u
  in
  Hashtbl.add unit_cache target_name [ u ]

let lookup_path _ap ~pages ~libs:_ ~hierarchy (kind, tag, path) =
  let module Env = Odoc_xref2.Env in
  let ( >>= ) x f = match x with Some x' -> f x' | None -> None in
  let page_path_to_path path =
    (* Turn [foo/bar] into [foo/page-bar.odoc]. *)
    let segs =
      match List.rev path with
      | [] -> []
      | name :: rest -> List.rev (("page-" ^ name ^ ".odoc") :: rest)
    in
    Fs.File.of_segs segs
  in
  let find_by_path ?root named_roots path =
    match Named_roots.find_by_path ?root named_roots ~path with
    | Ok x -> x
    | Error (NoPackage | NoRoot) -> None
  in
  let load_page path =
    match load_unit_from_file path with
    | Some (Odoc_file.Page_content page) -> Env.Path_page page
    | _ -> Env.Path_not_found
  in
  let find_page ?root path =
    (pages >>= fun pages -> find_by_path ?root pages path) |> function
    | Some path -> load_page path
    | None -> Env.Path_not_found
  in
  let find_page_in_hierarchy path =
    match hierarchy with
    | Some hierarchy -> (
        match Hierarchy.resolve_relative hierarchy path with
        | Ok path -> load_page path
        | Error `Escape_hierarchy ->
            Env.Path_not_found (* TODO: propagate more information *))
    | None -> Env.Path_not_found
  in
  match (kind, tag) with
  | `Page, `TCurrentPackage ->
      (* [path] is within the current package root. *)
      find_page (page_path_to_path path)
  | `Page, `TAbsolutePath -> (
      match path with
      | root :: path -> find_page ~root (page_path_to_path path)
      | [] -> Env.Path_not_found)
  | `Page, `TRelativePath -> find_page_in_hierarchy (page_path_to_path path)
  | _ -> Env.Path_not_found

type t = {
  important_digests : bool;
  ap : Accessible_paths.t;
  pages : Named_roots.t option;
  libs : Named_roots.t option;
  open_modules : string list;
  hierarchy : Hierarchy.t option;
}

let all_roots ?root named_roots =
  let all_files =
    match Named_roots.all_of ?root named_roots ~ext:"odocl" with
    | Ok x -> x
    | Error (NoPackage | NoRoot) -> []
  in
  let load page =
    match Odoc_file.load_root page with Error _ -> None | Ok root -> Some root
  in
  Odoc_utils.List.filter_map load all_files

let all_pages ?root ({ pages; _ } : t) =
  let filter (root : Odoc_model.Root.t) =
    match root with
    | {
     file = Page { title; _ };
     id = { iv = #Odoc_model.Paths.Identifier.Page.t_pv; _ } as id;
     _;
    } ->
        Some (id, title)
    | _ -> None
  in
  match pages with
  | None -> []
  | Some pages -> Odoc_utils.List.filter_map filter @@ all_roots ?root pages

let all_units ~library ({ libs; _ } : t) =
  let filter (root : Odoc_model.Root.t) =
    match root with
    | {
     file = Compilation_unit _;
     id = { iv = #Odoc_model.Paths.Identifier.RootModule.t_pv; _ } as id;
     _;
    } ->
        Some id
    | _ -> None
  in
  match libs with
  | None -> []
  | Some libs ->
      Odoc_utils.List.filter_map filter @@ all_roots ~root:library libs

type roots = {
  page_roots : (string * Fs.Directory.t) list;
  lib_roots : (string * Fs.Directory.t) list;
  current_lib : string option;
  current_package : string option;
  current_dir : Fs.Directory.t;
}

let create ~important_digests ~directories ~open_modules ~roots =
  let ap = Accessible_paths.create ~directories in
  let pages, libs, hierarchy =
    match roots with
    | None -> (None, None, None)
    | Some { page_roots; lib_roots; current_lib; current_package; current_dir }
      ->
        let pages = Named_roots.create ~current_root:current_package page_roots
        and libs = Named_roots.create ~current_root:current_lib lib_roots in
        let hierarchy =
          match Named_roots.find_by_path pages ~path:(Fpath.v ".") with
          | Ok (Some current_package) ->
              let current_package = Fs.Directory.of_file current_package in
              Some (Hierarchy.make ~current_package ~current_dir)
          | Ok None | Error _ -> None
        in
        (Some pages, Some libs, hierarchy)
  in
  { important_digests; ap; open_modules; pages; libs; hierarchy }

(** Helpers for creating xref2 env. *)

open Odoc_xref2

let build_compile_env_for_unit
    {
      important_digests;
      ap;
      open_modules = open_units;
      pages = _;
      libs = _;
      hierarchy = _;
    } m =
  add_unit_to_cache (Odoc_file.Unit_content m);
  let imports_map = build_imports_map m.imports in
  let lookup_unit = lookup_unit ~important_digests ~imports_map ap
  and lookup_page = lookup_page ap
  and lookup_impl = lookup_impl ap
  (* Do not implement [lookup_path] in compile mode, as that might return
     different results depending on the compilation order. *)
  and lookup_path _ = Env.Path_not_found in
  let resolver =
    { Env.open_units; lookup_unit; lookup_page; lookup_impl; lookup_path }
  in
  Env.env_of_unit m ~linking:false resolver

(** [important_digests] and [imports_map] only apply to modules. *)
let build ?(imports_map = StringMap.empty)
    { important_digests; ap; open_modules = open_units; pages; libs; hierarchy }
    =
  let lookup_unit = lookup_unit ~important_digests ~imports_map ap
  and lookup_page = lookup_page ap
  and lookup_impl = lookup_impl ap
  and lookup_path = lookup_path ap ~pages ~libs ~hierarchy in
  { Env.open_units; lookup_unit; lookup_page; lookup_impl; lookup_path }

let build_compile_env_for_impl t i =
  let imports_map =
    build_imports_map i.Odoc_model.Lang.Implementation.imports
  in
  let resolver = build ~imports_map t in
  Env.env_of_impl i resolver

let build_link_env_for_unit t m =
  add_unit_to_cache (Odoc_file.Unit_content m);
  let imports_map = build_imports_map m.imports in
  let resolver = build ~imports_map t in
  Env.env_of_unit m ~linking:true resolver

let build_link_env_for_impl t i =
  let imports_map =
    build_imports_map i.Odoc_model.Lang.Implementation.imports
  in
  let resolver = build ~imports_map t in
  Env.env_of_impl i resolver

let build_env_for_page t p =
  add_unit_to_cache (Odoc_file.Page_content p);
  let resolver = build { t with important_digests = false } in
  Env.env_of_page p resolver

let build_env_for_reference t =
  let resolver = build { t with important_digests = false } in
  Env.env_for_reference resolver

let lookup_page t target_name = lookup_page t.ap target_name

let resolve_import t target_name =
  let rec loop = function
    | [] -> None
    | path :: tl -> (
        match Odoc_file.load_root path with
        | Error _ -> loop tl
        | Ok root -> (
            match root.Odoc_model.Root.file with
            | Compilation_unit _ -> Some root
            | Impl _ | Page _ -> loop tl))
  in
  loop (Accessible_paths.find t.ap target_name)
