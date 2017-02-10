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

module Unit = OdocUnit
module Root = OdocRoot
module Fs = OdocFs

let failwithf fmt = Printf.ksprintf failwith fmt

type t = {
  expander    : Root.t DocOck.expander ;
  resolver    : Root.t DocOck.resolver ;
}

module Accessible_paths = struct
  type t = {
    root_map : Fs.File.t Root.Table.t;
    unit_map : (string, Root.t) Hashtbl.t;
    directories : Fs.Directory.t list;
  }

  let create ~directories =
    { root_map = Root.Table.create 42
    ; unit_map = Hashtbl.create 42
    ; directories }

  let find_file_by_name t name =
    let uname = name ^ ".odoc" in
    let lname = String.uncapitalize name ^ ".odoc" in
    let rec loop = function
      | [] -> raise Not_found
      | directory :: dirs ->
        let lfile = Fs.File.create ~directory ~name:lname in
        match Unix.stat (Fs.File.to_string lfile) with
        | _ -> lfile
        | exception Unix.Unix_error _ ->
          let ufile = Fs.File.create ~directory ~name:uname in
          match Unix.stat (Fs.File.to_string ufile) with
          | _ -> ufile
          | exception Unix.Unix_error _ ->
            loop dirs
    in
    loop t.directories

  let find_root ?digest t ~name =
    match Hashtbl.find t.unit_map name with
    | root -> root
    | exception Not_found ->
      let path = find_file_by_name t name in
      let root = Unit.read_root path in
      begin match digest with
      | Some d when Digest.compare d (Root.digest root) <> 0 ->
        Printf.eprintf
          "WARNING: digest of %s doesn't match the one excepted for unit %s\n%!"
          (Fs.File.to_string path) name
      | _ -> ()
      end;
      Hashtbl.add t.unit_map name root;
      Root.Table.add t.root_map root path;
      root

  let file_of_root t root =
    (* If we have a root, we called lookup before, so the root *is* in the
       table. *)
    Root.Table.find t.root_map root
end


type builder = Unit.t -> t

let create ?(important_digests=true) ~directories : builder =
  let ap = Accessible_paths.create ~directories in
  fun unit ->
    let lookup _ target_name : Root.t DocOck.lookup_result =
      let rec aux = function
        | [] ->
          if important_digests then DocOck.Not_found
          else begin
            match Accessible_paths.find_root ap ~name:target_name with
            | root -> Found root
            | exception Not_found -> Not_found
          end
        | import :: imports ->
          match import with
          | DocOckTypes.Unit.Import.Unresolved (name, digest) when name = target_name ->
            begin match digest with
            | None when important_digests -> Forward_reference
            | _ ->
              match Accessible_paths.find_root ap ~name:target_name ?digest with
              | root -> Found root
              | exception Not_found -> Not_found
            end
          | DocOckTypes.Unit.Import.Resolved root
            when Root.Unit.to_string (Root.unit root) = target_name ->
            Found root
          | _ ->
            aux imports
      in
      match aux unit.DocOckTypes.Unit.imports with
      | DocOck.Not_found ->
        let current_root = Unit.root unit in
        if target_name = Root.Unit.to_string (Root.unit current_root) then
          Found current_root
        else
          DocOck.Not_found
      | x -> x
    in
    let fetch root : Root.t DocOckTypes.Unit.t =
      let current_root = Unit.root unit in
      if Root.equal root current_root then
        unit
      else
        match Accessible_paths.file_of_root ap root with
        | path -> Unit.load path
        | exception Not_found ->
          Printf.eprintf "No unit for root: %s\n%!" (Root.to_string root);
          exit 2
    in
    let resolver = DocOck.build_resolver lookup fetch in
    let expander =
      (* CR trefis: what is the ~root param good for? *)
      let fetch ~root:_ root = fetch root in
      DocOck.build_expander (lookup ()) fetch
    in
    { expander; resolver }

let build builder unit =
  builder unit

let resolver t = t.resolver
let expander t = t.expander
