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

module Fs = OdocFs
module Unit = OdocUnit
module Root = OdocRoot

module Compile = struct
  type t = {
    unit_name : string;
    digest : Digest.t;
  }

  let name t = t.unit_name
  let digest t = t.digest
end

(*
module Html = struct
  type t = Fs.File.t
  let unit t = t
  let package t = t
end
   *)

let for_compile_step file =
  let input = Fs.File.to_string file in
  let cmi_infos = Cmi_format.read_cmi input in
  List.fold_left ~f:(fun acc -> function
    | _, None -> acc
    | unit_name, Some digest -> { Compile. unit_name; digest } :: acc
  ) ~init:[] cmi_infos.Cmi_format.cmi_crcs


(* TODO: for the moment this won't handle forward references properly (in documentation or
   just using -no-alias-deps). At some point we will want to walk the odoctree and list
   the root of every unresolved path (or reference rather?).
   Later. *)

module Hash_set : sig
  type t

  val create : unit -> t

  val add : t -> string -> unit

  val elements : t -> string list
end = struct
  type t = (string, unit) Hashtbl.t

  let add t elt = if Hashtbl.mem t elt then () else Hashtbl.add t elt ()

  let create () = Hashtbl.create 42

  let elements t = Hashtbl.fold (fun s () acc -> s :: acc) t []
end

module Path = DocOck.Paths.Path

let rec get_root set p =
  match p with
  | Path.Forward s -> Hash_set.add set s
  | Path.Root s -> Hash_set.add set s
  | Path.Dot (p, _) -> get_root set p
  | Path.Apply (p, _) -> get_root set p
  | Path.Resolved r ->
    (* Not sure if we should not go on here. If [p] is resolved then its root is
       probably imported. Then again, the path might simply have been imported
       from another file during expansion. *)
    ()

class ['a] find_roots set = object(self)
  inherit ['a] DocOckMaps.identifier
  inherit ['a] DocOckMaps.reference
  inherit ['a] DocOckMaps.path as super
  inherit ['a] DocOckMaps.fragment
  inherit ['a] DocOckMaps.types

  method root x = x


  method! path_resolved :
    type k. ('a, k) Path.Resolved.t -> ('a, k) Path.Resolved.t =
    fun x ->
      begin match x with
      | Path.Resolved.Canonical (_, p) -> get_root set p
      | _ -> ()
      end;
      super#path_resolved x
end

(* FIXME: return not just a unit name, but the name of the package it is in as well.
   Is this info in the imports list? Probably not. *)
let for_html_step input =
  let odoctree = Unit.load input in
  let deps = Hash_set.create () in
  let open DocOck.Types in
  List.iter odoctree.Unit.imports ~f:(fun import ->
    let import_name =
      match import with
      | Unit.Import.Resolved root ->
        (* TODO: return the digest as well so we stay consistent. *)
        Root.Unit.to_string (Root.unit root)
      | Unit.Import.Unresolved (unit_name, digest_opt) ->
        unit_name
    in
    Hash_set.add deps import_name
  );
  let find_roots = new find_roots deps in
  let _ = find_roots#unit odoctree in
  Hash_set.elements deps
