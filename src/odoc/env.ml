open Odoc_compat

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

module Accessible_paths = struct
  type t = {
    root_map : Fs.File.t Odoc_model.Root.Hash_table.t;
    file_map : (string, Odoc_model.Root.t list) Hashtbl.t;
    directories : Fs.Directory.t list;
  }

  let create ~directories =
    { root_map = Odoc_model.Root.Hash_table.create 42
    ; file_map = Hashtbl.create 42
    ; directories }

  let find_file_by_name t name =
    let uname = name ^ ".odoc" in
    let lname = String.uncapitalize_ascii name ^ ".odoc" in
    let rec loop acc = function
      | [] -> acc
      | directory :: dirs ->
        let lfile = Fs.File.create ~directory ~name:lname in
        match Unix.stat (Fs.File.to_string lfile) with
        | _ -> loop (lfile::acc) dirs
        | exception Unix.Unix_error _ ->
          let ufile = Fs.File.create ~directory ~name:uname in
          match Unix.stat (Fs.File.to_string ufile) with
          | _ -> loop (ufile::acc) dirs
          | exception Unix.Unix_error _ ->
            loop acc dirs
    in
    loop [] t.directories

  (* If there's only one possible file we've discovered in the search path
     we can check the digest right now. If there's more than one, we defer
     until further up the call stack *)
  let check_optional_digest ?digest filename (roots : Odoc_model.Root.t list) =
    match roots, digest with
    | [root], Some d when Digest.compare d root.digest <> 0 ->
      let warning = Odoc_model.Error.filename_only "Digest mismatch" filename in
      prerr_endline (Odoc_model.Error.to_string warning);
      roots
    | _ ->
      roots

  let find_root t ~filename =
    match Hashtbl.find t.file_map filename with
    | roots -> roots
    | exception Not_found ->
      let paths = find_file_by_name t filename in
      (* This could be the empty list *)
      let filter_map f l =
        List.fold_right (fun x acc -> match f x with | Some y -> y::acc | None -> acc) l []
      in
      let safe_read file =
        match Root.read file with
        | Ok root -> Some (root, file)
        | Error (`Msg msg) ->
          let warning = Odoc_model.Error.filename_only "%s" msg (Fs.File.to_string file) in
          prerr_endline (Odoc_model.Error.to_string warning);
          None
        | exception End_of_file ->
          let warning = Odoc_model.Error.filename_only "End_of_file while reading" (Fs.File.to_string file) in
          prerr_endline (Odoc_model.Error.to_string warning);
          None
      in
      let roots_paths = filter_map safe_read paths in
      let roots = List.map fst roots_paths in
      Hashtbl.add t.file_map filename roots;
      List.iter (fun (root, path) ->
        Odoc_model.Root.Hash_table.add t.root_map root path) roots_paths;
      roots

  let file_of_root t root =
    try Odoc_model.Root.Hash_table.find t.root_map root
    with Not_found ->
      let _roots =
        match root.file with
        | Page page_name ->
          let filename = "page-" ^ page_name in
          check_optional_digest ~digest:root.digest filename @@ find_root t ~filename
        | Compilation_unit { name; _ } ->
          check_optional_digest ~digest:root.digest name @@ find_root t ~filename:name
      in
      Odoc_model.Root.Hash_table.find t.root_map root
end

module StringMap = Map.Make(String)

let build_imports_map imports =
  List.fold_left
    (fun map import ->
       match import with
       | Odoc_model.Lang.Compilation_unit.Import.Unresolved (name, _) ->
         StringMap.add name import map
       | Odoc_model.Lang.Compilation_unit.Import.Resolved (_, name) ->
         StringMap.add (Odoc_model.Names.ModuleName.to_string name) import map)
    StringMap.empty imports

let lookup_unit ~important_digests ap target_name import_map =
  let handle_root (root : Odoc_model.Root.t) = match root.file with
    | Compilation_unit {hidden; _} -> Odoc_xref2.Env.Found {root; hidden}
    | Page _ -> assert false
  in
  let find_root ~digest =
    match Accessible_paths.find_root ap ~filename:target_name, digest with
    | [], _ ->
       Odoc_xref2.Env.Not_found
    | [r], _ -> handle_root r (* Already checked the digest, if one's been specified *)
    | r :: rs, None ->
      Printf.fprintf stderr "Warning, ambiguous lookup. Please wrap your libraries. Possible files:\n%!";
      let files_strs =
        List.map (fun root ->
          Accessible_paths.file_of_root ap root
          |> Fs.File.to_string
          |> Printf.sprintf "  %s") (r::rs)
      in
      prerr_endline (String.concat "\n" files_strs);
      (* We've not specified a digest, let's try the first one *)
      handle_root r
    | roots, Some d ->
      (* If we can't find a module that matches the digest, return Not_found *)
      try handle_root @@ List.find (fun root -> root.Odoc_model.Root.digest = d) roots
      with Not_found -> Odoc_xref2.Env.Not_found
  in
  match StringMap.find target_name import_map with
  | Odoc_model.Lang.Compilation_unit.Import.Unresolved (_, digest) -> begin
      match digest with
      | None when important_digests -> Odoc_xref2.Env.Forward_reference
      | _ -> find_root ~digest
    end
  | Odoc_model.Lang.Compilation_unit.Import.Resolved(root, _) -> begin
      match root.file with
      | Compilation_unit {hidden; _} -> Found {root; hidden}
      | Page _ -> assert false
    end
  | exception Not_found ->
    if important_digests then Odoc_xref2.Env.Not_found
    else find_root ~digest:None

let lookup_page ap target_name =
  match Accessible_paths.find_root ap ~filename:("page-" ^ target_name) with
  | [] -> None
  | [root] -> Some root
  | root :: _roots -> Some root

let fetch_page ap root =
  match Accessible_paths.file_of_root ap root with
  | path -> Page.load path
  | exception Not_found ->
    let msg =
      Printf.sprintf "No unit for root: %s\n%!" (Odoc_model.Root.to_string root)
    in
    Error (`Msg msg)

let fetch_unit ap root =
  match Accessible_paths.file_of_root ap root with
  | path -> Compilation_unit.load path
  | exception Not_found ->
    let msg =
      Printf.sprintf "No unit for root: %s\n%!" (Odoc_model.Root.to_string root)
    in
    Error (`Msg msg)


type t = Odoc_xref2.Env.resolver

type builder = [ `Unit of Compilation_unit.t | `Page of Page.t ] -> t

let create ?(important_digests=true) ~directories ~open_modules : builder =
  let ap = Accessible_paths.create ~directories in
  fun unit_or_page ->
    let lookup_unit =
    match unit_or_page with
    | `Page _ ->
      fun target_name ->
        lookup_unit ~important_digests:false ap target_name StringMap.empty
    | `Unit unit ->
        let imports_map =
          build_imports_map unit.Odoc_model.Lang.Compilation_unit.imports
        in
        fun target_name ->
          let lookup_result =
            lookup_unit
              ~important_digests
              ap
              target_name
              imports_map
          in
          match lookup_result with
          | Not_found -> begin
              let root = unit.root in
              match root.file with
              | Page _ -> assert false
              | Compilation_unit {name;hidden} when target_name = name ->
                Found { root; hidden }
              | Compilation_unit _ -> Not_found
            end
          | x -> x
    in
    let fetch_unit root : (Odoc_model.Lang.Compilation_unit.t, _) Result.result =
      match unit_or_page with
      | `Page _ -> fetch_unit ap root
      | `Unit unit ->
        let current_root = unit.root in
        if Odoc_model.Root.equal root current_root then
          Ok unit
        else
          fetch_unit ap root
    in
    let lookup_page target_name = lookup_page ap target_name in
    let fetch_page root : (Odoc_model.Lang.Page.t, _) Result.result =
      match unit_or_page with
      | `Unit _ -> fetch_page ap root
      | `Page page ->
        let current_root = page.Odoc_model.Lang.Page.root in
        if Odoc_model.Root.equal root current_root then
          Ok page
        else
          fetch_page ap root
    in
    Odoc_xref2.Compile.build_resolver open_modules lookup_unit fetch_unit lookup_page fetch_page

let build builder unit =
  builder unit
