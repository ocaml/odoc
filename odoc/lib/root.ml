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

module Digest = Digest

module Package = struct
  type t = string
  let create s = s
  let to_string s = s

  module Table = Hashtbl.Make(struct
    type nonrec t = t
    let equal : t -> t -> bool = (=)
    let hash : t -> int = Hashtbl.hash
  end)
end

module Odoc_file = struct
  type t =
    | Page of string
    | Unit of { name : string; hidden : bool }

  let create_unit ~force_hidden name =
    let hidden = force_hidden || DocOck.Paths.contains_double_underscore name in
    Unit { name; hidden }

  let create_page name = Page name

  let name = function
    | Page name
    | Unit { name; _ } -> name

  let kind = function
    | Page _ -> "page"
    | Unit _ -> "unit"
end

module T = struct
  type t = {
    package : Package.t;
    file    : Odoc_file.t;
    digest  : Digest.t;
  }

  let digest t = t.digest

  let equal : t -> t -> bool = (=)
  let hash  : t -> int       = Hashtbl.hash
end

include T

let to_string t = Printf.sprintf "%s::%s" t.package (Odoc_file.name t.file)

let create ~package ~file ~digest = { package; file; digest }

let file t = t.file
let package t = t.package

module Xml = struct
  let parse i =
    begin match Xmlm.input i with
    | `El_start ((_, "root_description"), _) -> ()
    | _ -> assert false
    end;
    let package = ref "" in
    let file = ref (Odoc_file.Page "") in
    let digest = ref "" in
    let get_elt () =
      match Xmlm.input i, Xmlm.input i, Xmlm.input i with
      | `El_start ((_, name), _), `Data value, `El_end ->
        begin match name with
        | "package" -> package := value
        | "unit" -> file := Odoc_file.create_unit ~force_hidden:false value
        | "page" -> file := Odoc_file.create_page value
        | "digest" -> digest := (Digest.from_hex value)
        | _ -> assert false
        end
      | _ -> assert false
    in
    get_elt ();
    get_elt ();
    get_elt ();
    begin match Xmlm.input i with
    | `El_end -> ()
    | _ -> assert false
    end;
    create ~package:!package ~file:!file ~digest:!digest

  let fold =
    let make_tag name = (("", name), []) in
    let f output acc root =
      let flipped sign acc = output acc sign in
      acc
      |> flipped (`El_start (make_tag "root_description"))
      |> flipped (`El_start (make_tag "package"))
      |> flipped (`Data root.package)
      |> flipped `El_end
      |> flipped (`El_start (make_tag (Odoc_file.kind root.file)))
      |> flipped (`Data (Odoc_file.name root.file))
      |> flipped `El_end
      |> flipped (`El_start (make_tag "digest"))
      |> flipped (`Data (Digest.to_hex root.digest))
      |> flipped `El_end
      |> flipped `El_end
    in
    { DocOckXmlFold. f }
end

module Table = Hashtbl.Make(T)

let magic = "odoc-%%VERSION%%"

let load file ic =
  let m = really_input_string ic (String.length magic) in
  if m = magic then
    Marshal.from_channel ic
  else (
    Printf.eprintf "%s: invalid magic number %S, expected %S\n%!"
      file m magic;
    exit 1
  )

let save oc t =
  output_string oc magic;
  Marshal.to_channel oc t []

let read file =
  let file = Fs.File.to_string file in
  let ic = open_in file in
  let root = load file ic in
  close_in ic;
  root
