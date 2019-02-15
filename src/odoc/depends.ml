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

open StdLabels

module Compile = struct
  type t = {
    unit_name : string;
    digest : Digest.t;
  }

  let name t = t.unit_name
  let digest t = t.digest
end

let add_dep acc = function
| _, None -> acc (* drop module aliases *)
| unit_name, Some digest ->  { Compile.unit_name; digest } :: acc

let for_compile_step_cmt file =
  let cmt_infos = Cmt_format.read_cmt (Fs.File.to_string file) in
  List.fold_left ~f:add_dep ~init:[] cmt_infos.Cmt_format.cmt_imports

let for_compile_step_cmi_or_cmti file =
  let cmi_infos = Cmi_format.read_cmi (Fs.File.to_string file) in
  List.fold_left ~f:add_dep ~init:[] cmi_infos.Cmi_format.cmi_crcs

let for_compile_step file = match Fs.File.has_ext "cmt" file with
| true -> for_compile_step_cmt file
| false -> for_compile_step_cmi_or_cmti file

module Hash_set : sig
  type t

  val create : unit -> t

  val add : t -> Model.Root.t -> unit

  val elements : t -> Model.Root.t list
end = struct
  type t = unit Model.Root.Hash_table.t

  let add t elt =
    if Model.Root.Hash_table.mem t elt then
      ()
    else
      Model.Root.Hash_table.add t elt ()

  let create () = Model.Root.Hash_table.create 42

  let elements t = Model.Root.Hash_table.fold (fun s () acc -> s :: acc) t []
end

let deps_of_odoc_file ~deps input = match (Root.read input).file with
| Page _ -> () (* XXX something should certainly be done here *)
| Compilation_unit _ ->
    let odoctree = Compilation_unit.load input in
    List.iter odoctree.Model.Lang.Compilation_unit.imports ~f:(fun import ->
        match import with
        | Model.Lang.Compilation_unit.Import.Unresolved _  -> ()
        | Model.Lang.Compilation_unit.Import.Resolved root ->
            Hash_set.add deps root
      )

let for_html_step pkg_dir =
  let deps = Hash_set.create () in
  let add_deps () file = deps_of_odoc_file ~deps file in
  Fs.Directory.fold_files_rec ~ext:".odoc" add_deps () pkg_dir;
  Hash_set.elements deps
