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

type env_unit = [ `Module of Compilation_unit.t | `Page of Page.t ]
(** In this module, a unit is either a module or a page. A module is a
    [Compilation_unit]. *)

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

let unit_name (u : env_unit) =
  let open Odoc_model in
  let root = match u with `Page p -> p.root | `Module m -> m.root in
  let (Page name | Compilation_unit { name; _ }) = root.Root.file in
  name

let load_unit_from_file file =
  let file = Fs.File.to_string file in
  let ic = open_in_bin file in
  let res =
    try
      match Root.load file ic with
      | Error _ as e -> e
      | Ok root ->
          Ok
            (match root.Odoc_model.Root.file with
            | Page _ -> `Page (Marshal.from_channel ic)
            | Compilation_unit _ -> `Module (Marshal.from_channel ic))
    with exn ->
      let msg =
        Printf.sprintf "Error while unmarshalling %S: %s\n%!" file
          (match exn with Failure s -> s | _ -> Printexc.to_string exn)
      in
      Error (`Msg msg)
  in
  close_in ic;
  res

(** TODO: Propagate warnings instead of printing. *)
let load_units_from_files paths =
  let safe_read file acc =
    match load_unit_from_file file with
    | Ok u -> u :: acc
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

let lookup_module_with_digest ap target_name digest =
  let module_that_match_digest = function
    | `Module m
      when Digest.compare m.Odoc_model.Lang.Compilation_unit.digest digest = 0
      ->
        Some m
    | _ -> None
  in
  let units = load_units_from_name ap target_name in
  match find_map module_that_match_digest units with
  | Some (m, _) -> Odoc_xref2.Env.Found m
  | None -> Not_found

(** Lookup a module matching a name. If there is more than one result, report on
    stderr and return the first one.

    TODO: Correctly propagate warnings instead of printing. *)
let lookup_module_by_name ap target_name =
  let first_module = function `Module m -> Some m | `Page _ -> None in
  let units = load_units_from_name ap target_name in
  match find_map first_module units with
  | Some (m, []) -> Odoc_xref2.Env.Found m
  | Some (m, (_ :: _ as ambiguous)) ->
      Printf.fprintf stderr
        "Warning, ambiguous lookup. Please wrap your libraries. Possible files:\n\
         %!";
      let files_strs =
        List.map
          (fun u -> Printf.sprintf "  %s" (unit_name u))
          (`Module m :: ambiguous)
      in
      prerr_endline (String.concat "\n" files_strs);
      Found m
  | None -> Not_found

(** Lookup a module. First looks into [imports_map] then searches into the paths. *)
let lookup_unit ~important_digests ~imports_map ap target_name =
  match StringMap.find target_name imports_map with
  | Odoc_model.Lang.Compilation_unit.Import.Unresolved (_, Some digest) ->
      lookup_module_with_digest ap target_name digest
  | Unresolved (_, None) ->
      if important_digests then Odoc_xref2.Env.Forward_reference
      else lookup_module_by_name ap target_name
  | Resolved (root, _) -> lookup_module_with_digest ap target_name root.digest
  | exception Not_found ->
      if important_digests then Odoc_xref2.Env.Not_found
      else lookup_module_by_name ap target_name

(** Lookup a page.

    TODO: Warning on ambiguous lookup. *)
let lookup_page ap target_name =
  let target_name = "page-" ^ target_name in
  let is_page = function `Page p -> Some p | `Module _ -> None in
  let units = load_units_from_name ap target_name in
  match find_map is_page units with Some (p, _) -> Some p | None -> None

type t = Odoc_xref2.Env.resolver

type builder = env_unit -> t

(** Add the current unit to the cache. No need to load other units with the same
    name. *)
let add_unit_to_cache u = Hashtbl.add unit_cache (unit_name u) [ u ]

let create ?(important_digests = true) ~directories ~open_modules =
  let ap = Accessible_paths.create ~directories in
  fun unit_or_page ->
    add_unit_to_cache unit_or_page;
    let lookup_unit =
      match unit_or_page with
      | `Page _ ->
          lookup_unit ~important_digests:false ~imports_map:StringMap.empty ap
      | `Module current_m ->
          let imports_map = build_imports_map current_m in
          lookup_unit ~important_digests ~imports_map ap
    and lookup_page = lookup_page ap in
    Odoc_xref2.Compile.build_resolver open_modules lookup_unit lookup_page

let build_from_module builder m = builder (`Module m)

let build_from_page builder p = builder (`Page p)
