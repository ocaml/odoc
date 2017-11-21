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
    | Compilation_unit of { name : string; hidden : bool }

  let create_unit ~force_hidden name =
    let hidden =
      force_hidden || Paths.contains_double_underscore name in
    Compilation_unit { name; hidden }

  let create_page name = Page name

  let name = function
    | Page name
    | Compilation_unit { name; _ } -> name

  let kind = function
    | Page _ -> "page"
    | Compilation_unit _ -> "unit"
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

module Table = Hashtbl.Make(T)
