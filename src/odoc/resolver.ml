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

open Odoc_utils
open ResultMonad

type named_root = string * Fs.Directory.t
module Named_roots : sig
  type t

  type error = NoPackage | NoRoot

  type input = { name : string; dir : Fs.Directory.t }

  val create : input list -> current_root:named_root option -> t

  val current_root : t -> Fs.Directory.t option

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

  type t = { table : (string, pkg) Hashtbl.t; current_root : named_root option }

  type input = { name : string; dir : Fs.Directory.t }

  type error = NoPackage | NoRoot

  let hashtbl_find_opt cache package =
    match Hashtbl.find cache package with
    | x -> Some x
    | exception Not_found -> None

  let create (pkglist : input list) ~current_root =
    let cache = Hashtbl.create 42 in
    List.iter
      (fun { name = pkgname; dir = root } ->
        let flat = Unvisited root
        and hierarchical = (Hashtbl.create 42, root) in
        Hashtbl.add cache pkgname { flat; hierarchical })
      pkglist;
    { current_root; table = cache }

  let current_root t = Option.map snd t.current_root

  let find_by_path ?root { table = cache; current_root; _ } ~path =
    let path = Fpath.normalize path in
    let root =
      match (root, current_root) with
      | Some pkg, _ | None, Some (pkg, _) -> Ok pkg
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

  let find_by_name ?root { table = cache; current_root; _ } ~name =
    let package =
      match (root, current_root) with
      | Some pkg, _ | None, Some (pkg, _) -> Ok pkg
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
end

let () = ignore Named_roots.find_by_name [@warning "-5"]

module Accessible_paths : sig
  type t

  val create :
    directories:Fs.Directory.t list ->
    libraries:(string * Fs.Directory.t) list ->
    t
  (** Reads each directory once. [libraries] are the libraries named with [-L],
      each with its directory. *)

  val find : t -> string -> (string list * Fs.File.t) list
  (** The files of the given name, each with the libraries whose directory holds
      it. *)
end = struct
  type t = (string, string list * Fpath.t) Hashtbl.t

  let create ~directories ~libraries =
    let unit_cache = Hashtbl.create 42 in
    List.iter
      (fun directory ->
        let libs =
          List.filter_map
            (fun (lib, dir) ->
              if Fs.Directory.compare dir directory = 0 then Some lib else None)
            libraries
        in
        try
          let files = Sys.readdir (Fs.Directory.to_string directory) in
          Array.iter
            (fun file ->
              let file = Fpath.v file in
              if Fpath.has_ext "odoc" file then
                Hashtbl.add unit_cache
                  (Astring.String.Ascii.capitalize
                     (file |> Fpath.rem_ext |> Fpath.basename))
                  (libs, Fs.File.append directory file))
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

  val make : hierarchy_root:Fs.Directory.t -> current_dir:Fs.Directory.t -> t

  val resolve_relative : t -> Fs.File.t -> (Fs.File.t, error) result
  (** [resolve_relative h relpath] resolve [relpath] relatively to the current
      directory, making sure not to escape the hierarchy. *)
end = struct
  type t = { hierarchy_root : Fs.Directory.t; current_dir : Fs.Directory.t }

  type error = [ `Escape_hierarchy ]

  let make ~hierarchy_root ~current_dir = { hierarchy_root; current_dir }

  let resolve_relative t relpath =
    let path = Fs.File.append t.current_dir relpath in
    if Fs.Directory.contains ~parentdir:t.hierarchy_root path then Ok path
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
    | Asset_content { root; _ } ) =
  root_name root

let unit_cache = Hashtbl.create 42

let load_unit_from_file path =
  try Hashtbl.find unit_cache path
  with Not_found ->
    let r = Odoc_file.load path >>= fun u -> Ok u.content in
    Hashtbl.add unit_cache path r;
    r

let self = ref None

(* The files a lookup by name may choose between: those in the libraries of
   [scope], each in the directory its [-L] names, or all of them. Neither
   prefers one file to another: the lookup warns when several remain. *)
let within (scope : Odoc_xref2.Env.Library_scope.t) found =
  match scope with
  | Scoped libraries ->
      List.filter
        (fun (libs, _) -> List.exists (fun lib -> List.mem lib libraries) libs)
        found
  | Any -> found

(** The units in the files [found], within [scope]. The unit being compiled or
    linked stands for its own file. *)
let load_units =
  let safe_read file acc =
    match load_unit_from_file file with
    | Ok u -> u :: acc
    | Error (`Msg msg) ->
        (* TODO: Propagate warnings instead of printing. *)
        let warning =
          Odoc_model.Error.filename_only "%s" msg (Fs.File.to_string file)
        in
        prerr_endline (Odoc_model.Error.to_string warning);
        acc
  in
  let check_self name =
    match !self with
    | Some (n, unit) -> if n = name then Some unit else None
    | None -> None
  in
  let same_unit a b =
    let root_of
        ( Odoc_file.Unit_content { root; _ }
        | Page_content { root; _ }
        | Impl_content { root; _ }
        | Asset_content { root; _ } ) =
      root
    in
    (root_of a).Odoc_model.Root.id = (root_of b).id
  in
  fun (scope : Odoc_xref2.Env.Library_scope.t) target_name found ->
    let units =
      List.fold_right
        (fun (_, file) acc -> safe_read file acc)
        (within scope found) []
    in
    match (check_self target_name, scope) with
    | None, _ -> units
    | Some self, Any -> [ self ]
    | Some self, Scoped _ ->
        (* The unit being compiled or linked stands for its own file, which may
           be stale or not written yet. A scoped lookup that did not find the
           file is not looking where the unit is. *)
        List.map (fun u -> if same_unit self u then self else u) units

let load_units_from_name ap target_name =
  load_units Odoc_xref2.Env.Library_scope.any target_name
    (Accessible_paths.find ap target_name)

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
  | Some (m, _) -> Ok (Odoc_xref2.Env.Found m)
  | None -> Error `Not_found

(** Lookup a compilation unit matching a name. If there is more than one result,
    report on stderr and return the first one.

    TODO: Correctly propagate warnings instead of printing. *)
let choose_unit target_name units =
  let first_unit u =
    match u with
    | Odoc_file.Unit_content m -> Some m
    | Impl_content _ | Page_content _ | Asset_content _ -> None
  in
  let rec find_ambiguous tl =
    match find_map first_unit tl with
    | Some (m, tl) -> m :: find_ambiguous tl
    | None -> []
  in
  match find_map first_unit units with
  | Some (m, tl) ->
      (match find_ambiguous tl with
      | [] -> ()
      | ambiguous ->
          let ambiguous = m :: ambiguous in
          (* Name each unit with the pages it is in, which tell the units
             apart, as they have the same module name. *)
          let unit_path m =
            let (`Root (parent, name) | `LibraryParameter (parent, name)) =
              m.Odoc_model.Lang.Compilation_unit.id
            in
            let rec pages = function
              | None -> []
              | Some (`Page (p, n)) ->
                  pages p @ [ Odoc_model.Names.PageName.to_string n ]
            in
            String.concat ~sep:"/"
              (pages parent @ [ Odoc_model.Names.ModuleName.to_string name ])
          in
          (* Sorted, so the warning does not depend on the order of the
             search path. *)
          let ambiguous =
            List.sort String.compare (List.map unit_path ambiguous)
          in
          let warning =
            Odoc_model.Error.filename_only
              "Ambiguous lookup. Possible units: %a"
              Format.(
                pp_print_list
                  ~pp_sep:(fun ppf () -> pp_print_string ppf ", ")
                  pp_print_string)
              ambiguous target_name
          in
          prerr_endline (Odoc_model.Error.to_string warning));
      Some m
  | None -> None

(** Look up a unit by name. A name the unit's imports record with a digest is
    matched by that digest along [ap], which holds the [-I] and the [-L]
    directories. Any other name is looked up by name, in [scope]. *)
let lookup_unit_by_name ~important_digests ~imports_map ~scope ap target_name =
  let by_name () =
    choose_unit target_name
      (load_units scope target_name (Accessible_paths.find ap target_name))
  in
  let of_option f =
    match f with
    | Some m -> Ok (Odoc_xref2.Env.Found m)
    | None -> Error `Not_found
  in
  match StringMap.find target_name imports_map with
  | Odoc_model.Lang.Compilation_unit.Import.Unresolved (_, Some digest) ->
      lookup_unit_with_digest ap target_name digest
  | Unresolved (_, None) ->
      if important_digests then Ok Odoc_xref2.Env.Forward_reference
      else of_option (by_name ())
  | Resolved (root, _) -> lookup_unit_with_digest ap target_name root.digest
  | exception Not_found ->
      if important_digests then Error `Not_found else of_option (by_name ())

(** Lookup a page.

    TODO: Warning on ambiguous lookup. *)
let lookup_page_by_name ap target_name =
  let target_name = "page-" ^ target_name in
  let is_page u =
    match u with
    | Odoc_file.Page_content p -> Some p
    | Impl_content _ | Unit_content _ | Asset_content _ -> None
  in
  let units = load_units_from_name ap target_name in
  match find_map is_page units with
  | Some (p, _) -> Ok p
  | None -> Error `Not_found

(* The unit [id] names, if exactly one file on the search path holds a unit
   with that identifier. Units compiled without [--parent-id] can share an
   identifier. Then the identifier does not say which unit it means, and the
   result is [None]. *)
let lookup_unit_by_id ap (id : Odoc_model.Paths.Identifier.RootModule.t) =
  let (`Root (_, name) | `LibraryParameter (_, name)) = id in
  load_units_from_name ap (Odoc_model.Names.ModuleName.to_string name)
  |> List.filter_map (function
       | Odoc_file.Unit_content m
         when Odoc_model.Paths.Identifier.RootModule.equal m.id id ->
           Some m
       | _ -> None)
  |> function
  | [ m ] -> Some m
  | [] | _ :: _ :: _ -> None

(** Look up an implementation by name, among the libraries in [scope]. Shapes
    name units, not digests, so the scope is what keeps a source link out of
    another library's implementation of the same name, such as another
    implementation of the same virtual library. dune forbids two of those in one
    closure, so the first one found is the only one. *)
let lookup_impl ~scope ap target_name =
  let target_name = "impl-" ^ Astring.String.Ascii.uncapitalize target_name in
  let is_impl u =
    match u with
    | Odoc_file.Impl_content p -> Some p
    | Page_content _ | Unit_content _ | Asset_content _ -> None
  in
  let units =
    load_units scope target_name (Accessible_paths.find ap target_name)
  in
  match find_map is_impl units with Some (p, _) -> Some p | None -> None

(** Add the current unit to the cache. No need to load other units with the same
    name. *)
let add_unit_to_cache u =
  let target_name =
    (match u with
    | Odoc_file.Page_content _ -> "page-"
    | Impl_content _ -> "impl-"
    | Unit_content _ -> ""
    | Asset_content _ -> "asset-")
    ^ unit_name u
  in
  self := Some (target_name, u)

(** Resolve a path reference in the given named roots and hierarchy.
    [possible_unit_names] should return a list of possible file names for the
    given unit name. *)
let lookup_path ~possible_unit_names ~named_roots ~hierarchy (tag, path) :
    (Odoc_file.content, [ `Not_found ]) result =
  let open Odoc_utils.OptionMonad in
  let option_to_result = function Some p -> Ok p | None -> Error `Not_found in
  (* TODO: We might want to differentiate when the file is not found and when
     an unexpected error occurred. *)
  let handle_load_error = function Ok u -> Some u | Error (`Msg _) -> None in
  let ref_path_to_file_path path =
    match List.rev path with
    | [] -> []
    | name :: rest ->
        List.map
          (fun fname -> List.rev (fname :: rest) |> Fs.File.of_segs)
          (possible_unit_names name)
  in
  let find_by_path ?root named_roots path =
    match Named_roots.find_by_path ?root named_roots ~path with
    | Ok x -> x
    | Error (NoPackage | NoRoot) -> None
  in
  let find_in_named_roots ?root path =
    named_roots >>= fun named_roots ->
    find_by_path ?root named_roots path >>= fun path ->
    load_unit_from_file path |> handle_load_error
  in
  let find_in_hierarchy path =
    hierarchy >>= fun hierarchy ->
    match Hierarchy.resolve_relative hierarchy path with
    | Ok path -> load_unit_from_file path |> handle_load_error
    | Error `Escape_hierarchy -> None (* TODO: propagate more information *)
  in
  match tag with
  | `TCurrentPackage ->
      (* [path] is within the current package root. *)
      ref_path_to_file_path path
      |> List.find_map find_in_named_roots
      |> option_to_result
  | `TAbsolutePath ->
      (match path with
      | root :: path ->
          ref_path_to_file_path path
          |> List.find_map (find_in_named_roots ~root)
      | [] -> None)
      |> option_to_result
  | `TRelativePath ->
      ref_path_to_file_path path
      |> List.find_map find_in_hierarchy
      |> option_to_result

let lookup_asset_by_path ~pages ~hierarchy path =
  let possible_unit_names name = [ "asset-" ^ name ^ ".odoc" ] in
  match lookup_path ~possible_unit_names ~named_roots:pages ~hierarchy path with
  | Ok (Odoc_file.Asset_content asset) -> Ok asset
  | Ok _ -> Error `Not_found (* TODO: Report is not an asset. *)
  | Error _ as e -> e

let lookup_page_by_path ~pages ~hierarchy path =
  let possible_unit_names name = [ "page-" ^ name ^ ".odoc" ] in
  match lookup_path ~possible_unit_names ~named_roots:pages ~hierarchy path with
  | Ok (Odoc_file.Page_content page) -> Ok page
  | Ok _ -> Error `Not_found (* TODO: Report is not a page. *)
  | Error _ as e -> e

let lookup_unit_by_path ~libs ~hierarchy path =
  let possible_unit_names name =
    Astring.String.Ascii.
      [ capitalize name ^ ".odoc"; uncapitalize name ^ ".odoc" ]
  in
  match lookup_path ~possible_unit_names ~named_roots:libs ~hierarchy path with
  | Ok (Odoc_file.Unit_content u) -> Ok u
  | Ok _ -> Error `Not_found (* TODO: Report is not a module. *)
  | Error _ as e -> e

let lookup_page ap ~pages ~hierarchy = function
  | `Path p -> lookup_page_by_path ~pages ~hierarchy p
  | `Name n -> lookup_page_by_name ap n

let lookup_asset ~pages ~hierarchy = function
  | `Path p -> lookup_asset_by_path ~pages ~hierarchy p
  | `Name _ -> failwith "TODO"

type t = {
  important_digests : bool;
  ap : Accessible_paths.t;
      (** The search path for units: the [-I] and [-L] directories, the input's
          directory and the current package's top directory. *)
  page_ap : Accessible_paths.t;
      (** The search path for a page named without a path: the [-I] directories,
          the input's directory and the current package's top directory. *)
  pages : Named_roots.t option;
  libs : Named_roots.t option;
  open_modules : string list;
  current_dir : Fs.Directory.t option;
}

type roots = {
  page_roots : named_root list;
  lib_roots : named_root list;
  current_lib : named_root option;
  current_package : named_root option;
  current_dir : Fs.Directory.t;
}

let create ~important_digests ~directories ~open_modules ~roots =
  let pages, libs, current_dir, page_directories, directories =
    match roots with
    | None -> (None, None, None, directories, directories)
    | Some { page_roots; lib_roots; current_lib; current_package; current_dir }
      ->
        let prepare roots =
          List.map (fun (name, dir) -> { Named_roots.name; dir }) roots
        in
        (* The search path: the [-I] directories, the input's own directory
           and that of the current package, and for units the [-L]
           directories. *)
        let directories =
          match current_package with
          | None -> current_dir :: directories
          | Some (_pkg, dir) -> current_dir :: dir :: directories
        in
        let page_directories = List.sort_uniq Fs.Directory.compare directories
        and directories =
          List.sort_uniq Fs.Directory.compare
            (directories @ List.map snd lib_roots)
        in
        let lib_roots = prepare lib_roots in
        let page_roots = prepare page_roots in
        let pages = Named_roots.create ~current_root:current_package page_roots
        and libs = Named_roots.create ~current_root:current_lib lib_roots in
        (Some pages, Some libs, Some current_dir, page_directories, directories)
  in
  let libraries = match roots with None -> [] | Some r -> r.lib_roots in
  let ap = Accessible_paths.create ~directories ~libraries
  and page_ap =
    Accessible_paths.create ~directories:page_directories ~libraries:[]
  in
  { important_digests; ap; page_ap; open_modules; pages; libs; current_dir }

(** Helpers for creating xref2 env. *)

open Odoc_xref2

let build_compile_env_for_unit
    {
      important_digests;
      ap;
      page_ap = _;
      open_modules = open_units;
      pages = _;
      libs = _;
      current_dir = _;
    } m =
  add_unit_to_cache (Odoc_file.Unit_content m);
  let imports_map = build_imports_map m.imports in
  (* Do not implement [lookup_page] in compile mode, as that might return
     different results depending on the compilation order.
     On the other hand, [lookup_unit_by_name] is needed at compile time and the
     compilation order is known by the driver. *)
  (* The resolver of [odoc compile] is not given the library names, so it
     cannot keep a lookup to a root's libraries. A root with no digest is left for
     link, unless [--resolve-fwd-refs] asks for it to be looked up now, and
     then it is looked up everywhere, as for a legacy unit. *)
  let lookup_unit_by_name ~scope:_ name =
    lookup_unit_by_name ~important_digests ~imports_map
      ~scope:Env.Library_scope.any ap name
  and lookup_unit_by_path = lookup_unit_by_path ~libs:None ~hierarchy:None
  and lookup_page _ = Error `Not_found
  and lookup_asset _ = Error `Not_found
  and lookup_impl = lookup_impl ~scope:Env.Library_scope.any ap in
  let shape_reducer = Odoc_xref2.Shape_reducer.make ~lookup_impl in
  let resolver =
    {
      Env.open_units;
      lookup_unit_by_name;
      lookup_unit_by_path;
      lookup_unit_by_id = lookup_unit_by_id ap;
      lookup_page;
      lookup_impl;
      lookup_asset;
      shape_reducer;
    }
  in
  Env.env_of_unit m ~linking:false resolver

(** [important_digests] and [imports_map] only apply to modules. *)
let build ?(imports_map = StringMap.empty) ?hierarchy_roots ~scope
    {
      important_digests;
      ap;
      page_ap;
      open_modules = open_units;
      pages;
      libs;
      current_dir;
    } =
  let hierarchy =
    let open OptionMonad in
    current_dir >>= fun current_dir ->
    hierarchy_roots >>= Named_roots.current_root >>= fun hierarchy_root ->
    Some (Hierarchy.make ~hierarchy_root ~current_dir)
  in
  (* A command that takes no [-L], such as [odoc html-url], knows no library
     names, so it cannot keep to a unit's libraries. It looks names up
     everywhere, as for a legacy unit. *)
  let scoped scope =
    match libs with None -> Env.Library_scope.any | Some _ -> scope
  in
  let lookup_unit_by_name ~scope name =
    lookup_unit_by_name ~important_digests ~imports_map ~scope:(scoped scope) ap
      name
  and lookup_unit_by_path = lookup_unit_by_path ~libs ~hierarchy
  and lookup_page = lookup_page page_ap ~pages ~hierarchy
  and lookup_asset = lookup_asset ~pages ~hierarchy
  and lookup_impl = lookup_impl ~scope:(scoped scope) ap in
  let shape_reducer = Odoc_xref2.Shape_reducer.make ~lookup_impl in
  {
    Env.open_units;
    lookup_unit_by_name;
    lookup_unit_by_path;
    lookup_unit_by_id = lookup_unit_by_id ap;
    lookup_page;
    lookup_impl;
    lookup_asset;
    shape_reducer;
  }

let build_compile_env_for_impl t i =
  let imports_map =
    build_imports_map i.Odoc_model.Lang.Implementation.imports
  in
  let resolver = build ~imports_map ~scope:Env.Library_scope.any t in
  Env.env_of_impl i resolver

let build_link_env_for_unit t m =
  add_unit_to_cache (Odoc_file.Unit_content m);
  let imports_map = build_imports_map m.imports in
  let scope =
    Env.Library_scope.of_libraries m.Odoc_model.Lang.Compilation_unit.libraries
  in
  let resolver = build ~imports_map ?hierarchy_roots:t.libs ~scope t in
  Env.env_of_unit m ~linking:true resolver

let build_link_env_for_impl t i =
  let imports_map =
    build_imports_map i.Odoc_model.Lang.Implementation.imports
  in
  let scope =
    Env.Library_scope.of_libraries i.Odoc_model.Lang.Implementation.libraries
  in
  let resolver = build ~imports_map ~scope t in
  Env.env_of_impl i resolver

let build_env_for_page t p =
  add_unit_to_cache (Odoc_file.Page_content p);
  let resolver =
    build ?hierarchy_roots:t.pages ~scope:Env.Library_scope.any
      { t with important_digests = false }
  in
  Env.env_of_page p resolver

let build_env_for_reference t =
  let resolver =
    build ~scope:Env.Library_scope.any { t with important_digests = false }
  in
  Env.env_for_reference resolver

let lookup_page t target_name =
  match lookup_page_by_name t.page_ap target_name with
  | Ok p -> Some p
  | Error `Not_found -> None

let resolve_import t target_name digest =
  let rec loop = function
    | [] -> None
    | path :: tl -> (
        match Odoc_file.load_root path with
        | Error _ -> loop tl
        | Ok root -> (
            match root.Odoc_model.Root.file with
            | Compilation_unit _
              when Digest.compare root.Odoc_model.Root.digest digest = 0 ->
                Some root
            | Compilation_unit _ | Impl _ | Page _ | Asset _ -> loop tl))
  in
  loop (List.map snd (Accessible_paths.find t.ap target_name))
