(*
 * Copyright (c) 2016 Thomas Refis <trefis@janestreet.com>
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

type directory = string
type file = string

let (^/) = Printf.sprintf "%s/%s"

module Directory = struct
  type t = directory

  let create ~parent ~name =
    let path = parent ^/ name in
    begin if not (Sys.file_exists path) then
      Unix.mkdir path 0o755
    else if not (Sys.is_directory path) then
      invalid_arg "not a directory"
    end;
    path

  let of_string s = s
  let to_string t = t

  let ls t = Sys.readdir t |> Array.to_list |> List.map ~f:((^/) t)
end

module File = struct
  type t = file

  let create ~directory ~name = directory ^/ name

  let of_string s = s
  let to_string t = t
end
