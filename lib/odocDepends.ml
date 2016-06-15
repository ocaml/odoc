open Odoc

module Compile = struct
  type t = {
    unit_name : string;
    digest : Digest.t;
  }

  let name t = t.unit_name
  let digest t = t.digest
end

(*
module Link = struct
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

(* CR trefis: for the moment this won't handle forward references properly (in
   documentation or just using -no-alias-deps).
   At some point we will want to walk the odoctree and list the root of every
   unresolved path.
   Later. *)
let for_link_step input =
  let odoctree = Unit.load input in
  let open DocOck.Types in
  List.map (function
    | Unit.Import.Resolved root ->
      (* TODO: return the digest as well so we stay consistent. *)
      Root.Unit.to_string (Root.unit root)
    | Unit.Import.Unresolved (unit_name, digest_opt) ->
      unit_name
  ) odoctree.Unit.imports
