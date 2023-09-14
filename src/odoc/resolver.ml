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

module Accessible_paths : sig
  type t

  val create : directories:Fs.Directory.t list -> t

  val find : t -> string -> Fs.File.t list
end = struct
  type t = { directories : Fs.Directory.t list }

  let create ~directories = { directories }

  let find t name =
    let uname = Astring.String.Ascii.capitalize name ^ ".odoc" in
    let lname = Astring.String.Ascii.uncapitalize name ^ ".odoc" in
    let rec loop acc = function
      | [] -> acc
      | directory :: dirs -> (
          let lfile = Fs.File.create ~directory ~name:lname in
          match Unix.stat (Fs.File.to_string lfile) with
          | _ -> loop (lfile :: acc) dirs
          | exception Unix.Unix_error _ -> (
              let ufile = Fs.File.create ~directory ~name:uname in
              match Unix.stat (Fs.File.to_string ufile) with
              | _ -> loop (ufile :: acc) dirs
              | exception Unix.Unix_error _ -> loop acc dirs))
    in
    loop [] t.directories
end

module StringMap = Map.Make (String)

let build_imports_map m =
  let imports = m.Odoc_model.Lang.Compilation_unit.imports in
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
    | Page_content _ | Source_tree_content _ -> None
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
    | Unit_content _ | Source_tree_content _ -> None
  in
  let units = load_units_from_name ap target_name in
  match find_map is_page units with Some (p, _) -> Some p | None -> None

(** Add the current unit to the cache. No need to load other units with the same
    name. *)
let add_unit_to_cache u =
  let target_name =
    (match u with
    | Odoc_file.Page_content _ -> "page-"
    | Unit_content _ -> ""
    | Source_tree_content _ -> "page-")
    ^ unit_name u
  in
  Hashtbl.add unit_cache target_name [ u ]

type t = {
  important_digests : bool;
  ap : Accessible_paths.t;
  open_modules : string list;
}

let create ~important_digests ~directories ~open_modules =
  let ap = Accessible_paths.create ~directories in
  { important_digests; ap; open_modules }

(** Helpers for creating xref2 env. *)

open Odoc_xref2

let build_compile_env_for_unit
    { important_digests; ap; open_modules = open_units } m =
  add_unit_to_cache (Odoc_file.Unit_content m);
  let imports_map = build_imports_map m in
  let lookup_unit = lookup_unit ~important_digests ~imports_map ap
  and lookup_page = lookup_page ap in
  let resolver = { Env.open_units; lookup_unit; lookup_page } in
  Env.env_of_unit m ~linking:false resolver

(** [important_digests] and [imports_map] only apply to modules. *)
let build ?(imports_map = StringMap.empty)
    { important_digests; ap; open_modules = open_units } =
  let lookup_unit = lookup_unit ~important_digests ~imports_map ap
  and lookup_page = lookup_page ap in
  { Env.open_units; lookup_unit; lookup_page }

let build_link_env_for_unit t m =
  add_unit_to_cache (Odoc_file.Unit_content m);
  let imports_map = build_imports_map m in
  let resolver = build ~imports_map t in
  Env.env_of_unit m ~linking:true resolver

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
            | Page _ -> loop tl))
  in
  loop (Accessible_paths.find t.ap target_name)
