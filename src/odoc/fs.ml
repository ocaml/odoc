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

open StdLabels

type directory = Fpath.t
type file = Fpath.t

module File = struct
  type t = file

  let dirname = Fpath.parent
  let basename = Fpath.base

  let set_ext e p = Fpath.set_ext e p
  let has_ext e p = Fpath.has_ext e p

  let create ~directory ~name =
    match Fpath.of_string name with
    | Error (`Msg e) -> invalid_arg ("Odoc.Fs.File.create: " ^ e)
    | Ok psuf -> Fpath.(normalize @@ directory // psuf)

  let to_string = Fpath.to_string
  let of_string s =
    match Fpath.of_string s with
    | Error (`Msg e) -> invalid_arg ("Odoc.Fs.File.of_string: " ^ e)
    | Ok p -> p

  let read = Bos.OS.File.read

  module Table = Hashtbl.Make(struct
      type nonrec t = t
      let equal = Fpath.equal
      let hash = Hashtbl.hash
    end)
end

module Directory = struct
  type t = directory

  let dirname = Fpath.parent
  let basename = Fpath.base

  let append = Fpath.append

  let make_path p name =
    match Fpath.of_string name with
    | Error _ as e -> e
    | Ok psuf -> Ok (Fpath.(normalize @@ to_dir_path @@ p // psuf))

  let reach_from ~dir path =
    match make_path dir path with
    | Error (`Msg e) -> invalid_arg ("Odoc.Fs.Directory.create: " ^ e)
    | Ok path ->
      let pstr = Fpath.to_string path in
      if Sys.file_exists pstr && not (Sys.is_directory pstr) then
        invalid_arg "Odoc.Fs.Directory.create: not a directory";
      path

  let mkdir_p dir =
    let mkdir d =
      try Unix.mkdir (Fpath.to_string d) 0o755 with
      | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
      | exn -> raise exn
    in
    let rec dirs_to_create p acc =
      if Sys.file_exists (Fpath.to_string p) then acc else
        dirs_to_create (Fpath.parent p) (p :: acc)
    in
    List.iter (dirs_to_create dir []) ~f:mkdir


  let to_string = Fpath.to_string
  let of_string s =
    match Fpath.of_string s with
    | Error (`Msg e) -> invalid_arg ("Odoc.Fs.Directory.of_string: " ^ e)
    | Ok p -> Fpath.to_dir_path p

  let ls t =
    let elts = Sys.readdir (to_string t) |> Array.to_list in
    List.fold_left elts ~init:[] ~f:(fun acc elt ->
      let file = File.create ~directory:t ~name:elt in
      if Fpath.is_file_path file then file :: acc else acc
    )

  module Table = Hashtbl.Make(struct
      type nonrec t = t
      let equal = Fpath.equal
      let hash = Hashtbl.hash
    end)
end
