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

module Unit = struct
  type t = { name : string; hidden : bool }

  let create ~force_hidden name =
    let hidden = force_hidden || DocOck.Paths.contains_double_underscore name in
    { name; hidden }
end

module T = struct
  type t = {
    package : Package.t;
    unit    : Unit.t;
    digest  : Digest.t;
  }

  let digest t = t.digest

  let equal : t -> t -> bool = (=)
  let hash  : t -> int       = Hashtbl.hash
end

include T

let to_string t = t.package ^ "::" ^ t.unit.name

let create ~package ~unit ~digest = { package; unit; digest }

let unit t = t.unit
let package t = t.package

module Xml = struct
  let parse i =
    begin match Xmlm.input i with
    | `El_start ((_, "root_description"), _) -> ()
    | _ -> assert false
    end;
    let package = ref "" in
    let unit = ref "" in
    let digest = ref "" in
    let get_elt () =
      match Xmlm.input i, Xmlm.input i, Xmlm.input i with
      | `El_start ((_, name), _), `Data value, `El_end ->
        begin match name with
        | "package" -> package := value
        | "unit" -> unit := value
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
    create ~package:!package ~unit:(Unit.create ~force_hidden:false !unit)
      ~digest:!digest

  let fold =
    let make_tag name = (("", name), []) in
    let f output acc root =
      let flipped sign acc = output acc sign in
      acc
      |> flipped (`El_start (make_tag "root_description"))
      |> flipped (`El_start (make_tag "package"))
      |> flipped (`Data root.package)
      |> flipped `El_end
      |> flipped (`El_start (make_tag "unit"))
      |> flipped (`Data root.unit.name)
      |> flipped `El_end
      |> flipped (`El_start (make_tag "digest"))
      |> flipped (`Data (Digest.to_hex root.digest))
      |> flipped `El_end
      |> flipped `El_end
    in
    { DocOckXmlFold. f }
end

module Table = Hashtbl.Make(T)
