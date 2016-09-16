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

(* CR trefis: for the moment this won't handle forward references properly (in
   documentation or just using -no-alias-deps).
   At some point we will want to walk the odoctree and list the root of every
   unresolved path.
   Later. *)
let for_html_step input =
  let odoctree = Unit.load input in
  let open DocOck.Types in
  List.map (function
    | Unit.Import.Resolved root ->
      (* TODO: return the digest as well so we stay consistent. *)
      Root.Unit.to_string (Root.unit root)
    | Unit.Import.Unresolved (unit_name, digest_opt) ->
      unit_name
  ) odoctree.Unit.imports
