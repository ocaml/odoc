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

open Odoc_utils
open StdLabels

type directory = Fpath.t

type file = Fpath.t

let mkdir_p dir =
  let mkdir d =
    try Unix.mkdir (Fpath.to_string d) 0o755 with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
    | exn -> raise exn
  in
  let rec dirs_to_create p acc =
    if Sys.file_exists (Fpath.to_string p) then acc
    else dirs_to_create (Fpath.parent p) (p :: acc)
  in
  List.iter (dirs_to_create (Fpath.normalize dir) []) ~f:mkdir

module File = struct
  type t = file

  let dirname = Fpath.parent

  let basename = Fpath.base

  let append = Fpath.append

  let set_ext e p = Fpath.set_ext e p

  let has_ext e p = Fpath.has_ext e p

  let get_ext e = Fpath.get_ext e

  let create ~directory ~name =
    match Fpath.of_string name with
    | Error (`Msg e) -> invalid_arg ("Odoc.Fs.File.create: " ^ e)
    | Ok psuf -> Fpath.(normalize @@ (directory // psuf))

  let to_string = Fpath.to_string

  let of_string s =
    match Fpath.of_string s with
    | Error (`Msg e) -> invalid_arg ("Odoc.Fs.File.of_string: " ^ e)
    | Ok p -> p

  let read file =
    let input_one_shot len ic =
      let buf = Bytes.create len in
      really_input ic buf 0 len;
      close_in ic;
      Ok (Bytes.unsafe_to_string buf)
    in
    let input_stream file ic =
      let bsize =
        65536
        (* IO_BUFFER_SIZE *)
      in
      let buf = Buffer.create bsize in
      let rec loop () =
        match Buffer.add_channel buf ic bsize with
        | () -> loop ()
        | exception End_of_file -> Ok (Buffer.contents buf)
        | exception Failure _ ->
            Error (`Msg (Printf.sprintf "%s: input too large" file))
      in
      loop ()
    in
    try
      let file = Fpath.to_string file in
      let with_ic k =
        if file = "-" then k stdin else Io_utils.with_open_in_bin file k
      in
      with_ic @@ fun ic ->
      match in_channel_length ic with
      | 0 (* e.g. stdin or /dev/stdin *) -> input_stream file ic
      | len when len <= Sys.max_string_length -> input_one_shot len ic
      | len ->
          let err = Printf.sprintf "%s: file too large (%d bytes)" file len in
          Error (`Msg err)
    with Sys_error e -> Error (`Msg e)

  let copy ~src ~dst =
    try
      Io_utils.with_open_in_bin (Fpath.to_string src) (fun ic ->
          mkdir_p (dirname dst);
          Io_utils.with_open_out_bin (Fpath.to_string dst) (fun oc ->
              let len = 65536 in
              let buf = Bytes.create len in
              let rec loop () =
                let read = input ic buf 0 len in
                if read > 0 then (
                  output oc buf 0 read;
                  loop ())
              in
              Ok (loop ())))
    with Sys_error e -> Error (`Msg e)

  let exists file = Sys.file_exists (Fpath.to_string file)

  let rec of_segs_tl acc = function
    | [] -> acc
    | hd :: tl -> of_segs_tl (Fpath.( / ) acc hd) tl

  let of_segs = function
    | [] -> invalid_arg "Fs.File.of_segs"
    | "" :: rest -> of_segs_tl (Fpath.v "/") rest
    | first :: rest -> of_segs_tl (Fpath.v first) rest
end

module Directory = struct
  type t = directory

  let dirname = Fpath.parent

  let basename = Fpath.base

  let append = Fpath.append

  let contains ~parentdir f = Fpath.is_rooted ~root:parentdir f

  let compare = Fpath.compare

  let mkdir_p dir = mkdir_p dir

  let to_string = Fpath.to_string

  let to_fpath x = x

  let of_string s =
    match Fpath.of_string s with
    | Error (`Msg e) -> invalid_arg ("Odoc.Fs.Directory.of_string: " ^ e)
    | Ok p -> Fpath.to_dir_path p

  let of_file f = Fpath.to_dir_path f

  let fold_files_rec ?(ext = "") f acc d =
    let fold_non_dirs ext f acc files =
      let is_dir d = try Sys.is_directory d with Sys_error _ -> false in
      let has_ext ext file = Filename.check_suffix file ext in
      let dirs, files = List.partition ~f:is_dir files in
      let files = List.find_all ~f:(has_ext ext) files in
      let f acc fn = f acc (Fpath.v fn) in
      (List.fold_left ~f ~init:acc files, dirs)
    in
    let rec loop ext f acc = function
      | (d :: ds) :: up ->
          let rdir d =
            try Array.to_list (Sys.readdir d) with Sys_error _ -> []
          in
          let files = List.rev (List.rev_map ~f:(Filename.concat d) (rdir d)) in
          let acc, dirs = fold_non_dirs ext f acc files in
          loop ext f acc (dirs :: ds :: up)
      | [] :: up -> loop ext f acc up
      | [] -> acc
    in
    loop ext f acc [ [ Fpath.to_string d ] ]

  exception Stop_iter of msg

  let fold_files_rec_result ?ext f acc d =
    let f acc fn =
      match f acc fn with Ok acc -> acc | Error e -> raise (Stop_iter e)
    in
    try Ok (fold_files_rec ?ext f acc d)
    with Stop_iter (`Msg _ as e) -> Error e
end
