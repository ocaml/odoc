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

  type error = NoPackage

  val create : (string * Fs.Directory.t) list -> current_root:string -> t

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

  type t = { table : (string, pkg) Hashtbl.t; current_root : string }

  type error = NoPackage

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
    let root = match root with None -> current_root | Some pkg -> pkg in
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
        Fs.Directory.fold_files_rec_result (* bookmark *)
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
    let package = match root with None -> current_root | Some pkg -> pkg in
    match hashtbl_find_opt cache package with
    | Some { flat = Visited flat; _ } -> Ok (Hashtbl.find_all flat name)
    | Some ({ flat = Unvisited root; _ } as p) ->
        let flat = populate_flat_namespace ~root in
        Hashtbl.replace cache package { p with flat = Visited flat };
        Ok (Hashtbl.find_all flat name)
    | None -> Error NoPackage
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
let load_units_from_files paths =
  let safe_read file acc =
    match Odoc_file.load file with
    | Ok u -> u.content :: acc
    | Error (`Msg msg) ->
        let warning =
          Odoc_model.Error.filename_only "%s" msg (Fs.File.to_string file)
        in
        prerr_endline (Odoc_model.Error.to_string warning);
        acc
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
let lookup_unit ~important_digests ~imports_map ~libs ap target_name =
  ignore libs;
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
let lookup_page ~pages ap target_name =
  match (target_name.[0], pages) with
  (* WARNING: This is just a hack to be able to test resolving of references by
     package until a proper reference syntax is implemented: if a page reference
     starts with a [#], the reference is resolved in "test mode". *)
  | '#', Some pages -> (
      let reference =
        String.sub target_name 1 (String.length target_name - 1)
      in
      match Astring.String.cuts ~sep:"/" reference with
      | [ name ] -> (
          let name = "page-" ^ name ^ ".odoc" in
          match Named_roots.find_by_name ~root:"pkg" ~name pages with
          | Ok page -> (
              let units = load_units_from_files page in
              let is_page u =
                match u with
                | Odoc_file.Page_content p -> Some p
                | Impl_content _ | Unit_content _ | Source_tree_content _ ->
                    None
              in
              match find_map is_page units with
              | Some (p, _) -> Some p
              | None ->
                  Format.eprintf "%s\n"
                    ("Page not found by name: "
                    ^ string_of_int (List.length units));
                  None)
          | Error _ ->
              Format.eprintf "Not found by name";
              None)
      | [] -> assert false
      | "" :: root :: path -> (
          let path = Fs.File.of_string @@ String.concat "/" path in
          let filename = "page-" ^ Fpath.filename path ^ ".odoc" in
          let path = Fpath.( / ) (Fpath.parent path) filename in
          match Named_roots.find_by_path ~root ~path pages with
          | Ok None ->
              Format.eprintf "%s\n"
              @@ Format.asprintf
                   "Error during find by path: no file was found with this \
                    path: %a"
                   Fpath.pp path;
              None
          | Ok (Some page) -> (
              let units = load_units_from_files [ page ] in
              let is_page u =
                match u with
                | Odoc_file.Page_content p -> Some p
                | Impl_content _ | Unit_content _ | Source_tree_content _ ->
                    None
              in
              match find_map is_page units with
              | Some (p, _) -> Some p
              | None ->
                  failwith
                    ("Page not found by name: "
                    ^ string_of_int (List.length units)))
          | Error NoPackage ->
              Format.eprintf "%s\n"
              @@ "Error during find by path: no package was found with this \
                  name";
              None)
      | _ -> failwith "Relative references (a/b, ../a/b) are not yet tested")
  | _ -> (
      let target_name = "page-" ^ target_name in
      let is_page u =
        match u with
        | Odoc_file.Page_content p -> Some p
        | Impl_content _ | Unit_content _ | Source_tree_content _ -> None
      in
      let units = load_units_from_name ap target_name in
      match find_map is_page units with Some (p, _) -> Some p | None -> None)

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

type t = {
  important_digests : bool;
  ap : Accessible_paths.t;
  pages : Named_roots.t option;
  libs : Named_roots.t option;
  open_modules : string list;
}

type roots = {
  page_roots : (string * Fs.Directory.t) list;
  lib_roots : (string * Fs.Directory.t) list;
  current_root : string;
}

let create ~important_digests ~directories ~open_modules ~roots =
  let ap = Accessible_paths.create ~directories in
  let pages, libs =
    match roots with
    | None -> (None, None)
    | Some { page_roots; lib_roots; current_root } ->
        ( Some (Named_roots.create ~current_root page_roots),
          Some (Named_roots.create ~current_root lib_roots) )
  in
  { important_digests; ap; open_modules; pages; libs }

(** Helpers for creating xref2 env. *)

open Odoc_xref2

let build_compile_env_for_unit
    { important_digests; ap; open_modules = open_units; pages; libs } m =
  add_unit_to_cache (Odoc_file.Unit_content m);
  let imports_map = build_imports_map m.imports in
  let lookup_unit = lookup_unit ~important_digests ~imports_map ~libs ap
  and lookup_page = lookup_page ~pages ap
  and lookup_impl = lookup_impl ap in
  let resolver = { Env.open_units; lookup_unit; lookup_page; lookup_impl } in
  Env.env_of_unit m ~linking:false resolver

(** [important_digests] and [imports_map] only apply to modules. *)
let build ?(imports_map = StringMap.empty)
    { important_digests; ap; open_modules = open_units; pages; libs } =
  let lookup_unit = lookup_unit ~libs ~important_digests ~imports_map ap
  and lookup_page = lookup_page ~pages ap
  and lookup_impl = lookup_impl ap in
  { Env.open_units; lookup_unit; lookup_page; lookup_impl }

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

let lookup_page t target_name = lookup_page ~pages:t.pages t.ap target_name

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
