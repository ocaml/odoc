open Or_error

(* Copied from ocaml 5.0 String module *)
let string_starts_with ~prefix s =
  let open String in
  let len_s = length s and len_pre = length prefix in
  let rec aux i =
    if i = len_pre then true
    else if unsafe_get s i <> unsafe_get prefix i then false
    else aux (i + 1)
  in
  len_s >= len_pre && aux 0

let handle_file file ~f =
  if string_starts_with ~prefix:"impl-" (Fpath.filename file) then
    Odoc_file.load file |> function
    | Error _ as e -> e
    | Ok unit' -> (
        match unit' with
        | { Odoc_file.content = Impl_content impl; _ } -> Ok (Some (f impl))
        | _ -> Ok None)
  else Ok None

let fold_dirs ~dirs ~f ~init =
  dirs
  |> List.fold_left
       (fun acc dir ->
         acc >>= fun acc ->
         Fs.Directory.fold_files_rec_result ~ext:"odocl"
           (fun acc file ->
             file |> handle_file ~f:(f acc) >>= function
             | None -> Ok acc
             | Some acc -> Ok acc)
           acc dir)
       (Ok init)

let count ~dst ~warnings_options:_ directories include_hidden =
  let htbl = Odoc_occurrences.Table.v () in
  let f () (unit : Odoc_model.Lang.Implementation.t) =
    Odoc_occurrences.of_impl ~include_hidden unit htbl
  in
  fold_dirs ~dirs:directories ~f ~init:() >>= fun () ->
  Fs.Directory.mkdir_p (Fs.File.dirname dst);
  let oc = open_out_bin (Fs.File.to_string dst) in
  Marshal.to_channel oc htbl [];
  Ok ()

open Astring
open Or_error

let parse_input_file input =
  let is_sep = function '\n' | '\r' -> true | _ -> false in
  Fs.File.read input >>= fun content ->
  let files =
    String.fields ~empty:false ~is_sep content |> List.rev_map Fs.File.of_string
  in
  Ok files

let parse_input_files input =
  List.fold_left
    (fun acc file ->
      acc >>= fun acc ->
      parse_input_file file >>= fun files -> Ok (files :: acc))
    (Ok []) input
  >>= fun files -> Ok (List.concat files)

let aggregate files file_list ~warnings_options:_ ~dst =
  try
    parse_input_files file_list >>= fun new_files ->
    let files = files @ new_files in
    let from_file file : Odoc_occurrences.Table.t =
      let ic = open_in_bin (Fs.File.to_string file) in
      Marshal.from_channel ic
    in
    let occtbl =
      match files with
      | [] -> Odoc_occurrences.Table.v ()
      | file :: files ->
          let acc = from_file file in
          List.iter
            (fun file ->
              Odoc_occurrences.aggregate ~tbl:acc ~data:(from_file file))
            files;
          acc
    in
    let oc = open_out_bin (Fs.File.to_string dst) in
    Marshal.to_channel oc occtbl [];
    Ok ()
  with Sys_error s -> Error (`Msg s)
