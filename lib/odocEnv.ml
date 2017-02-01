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

let rec find_map' ~f = function
  | [] -> None
  | x :: xs ->
    match f x with
    | Some _ as res -> res
    | None -> find_map' ~f xs

let find_map ~f tbl =
  Hashtbl.fold (fun key value acc ->
    match acc with
    | None -> f key value
    | Some _ -> acc
  ) tbl None

let find_file_in_dir ?digest ~file (_, files) =
  let files = Lazy.force files in
  match Hashtbl.find files file with
  | exception Not_found -> None
  | file, lazy_root ->
    let root = Lazy.force lazy_root in
    match digest with
    | Some d when Digest.compare d (Root.digest root) <> 0 -> None
    | _ -> Some (file, root)

let find_odoc_file ~directories ?digest name =
  find_map' directories ~f:(find_file_in_dir ?digest ~file:name)

let odoc_file_from_root ~directories root =
  let pkg = Root.(Package.to_string @@ package root) in
  match List.find directories ~f:(fun (pkg_name, _) -> pkg_name = pkg) with
  | exception Not_found -> None
  | pkg_with_files ->
    find_file_in_dir pkg_with_files
      ~digest:Root.(digest root)
      ~file:Root.(Unit.to_string @@ unit root)

type builder = Unit.t -> t

let create ?(important_digests=true) ~directories : builder =
  let directories =
    List.map directories ~f:(fun dir ->
      let pkg_name = Filename.basename (Fs.Directory.to_string dir) in
      pkg_name, lazy (
        let tbl = Hashtbl.create 20 in
        let files = Fs.Directory.ls dir in
        List.iter files ~f:(fun file ->
          if Fs.File.has_ext "odoc" file then
            let unit_name =
              file
              |> Fs.File.basename
              |> Fs.File.to_string
              |> Filename.chop_extension
              |> String.capitalize
            in
            let root =
              lazy (
                Unit.load file
                |> Unit.root
              )
            in
            Hashtbl.add tbl unit_name (file, root)
        );
        tbl
      )
    )
  in
  fun unit ->
    let lookup _ target_name : Root.t DocOck.lookup_result =
      let rec aux = function
        | [] ->
          if important_digests then DocOck.Not_found
          else begin
            match find_odoc_file ~directories target_name with
            | None -> Not_found
            | Some (_, root) -> Found root
          end
        | import :: imports ->
          match import with
          | DocOckTypes.Unit.Import.Unresolved (name, digest_opt) when name = target_name ->
            begin match digest_opt with
            | None when important_digests -> Forward_reference
            | _ ->
              match find_odoc_file ~directories target_name ?digest:digest_opt with
              | None -> Not_found
              | Some (_, root) -> Found root
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
    let fetch root : Unit.t =
      let current_root = Unit.root unit in
      if Root.equal root current_root then
        unit
      else
        match odoc_file_from_root ~directories root with
        | None ->
          Printf.eprintf "No unit for root: %s\n%!" (Root.to_string root);
          exit 2
        | Some (file, _) -> Unit.load file
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
