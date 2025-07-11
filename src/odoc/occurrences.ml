open Odoc_utils
open ResultMonad

let handle_file file ~f =
  if String.is_prefix ~affix:"impl-" (Fpath.filename file) then
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
  Io_utils.marshal (Fs.File.to_string dst) htbl;
  Ok ()

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

let read_occurrences file : Odoc_occurrences.Table.t =
  Io_utils.unmarshal (Fpath.to_string file)

let aggregate files file_list ~strip_path ~warnings_options:_ ~dst =
  try
    parse_input_files file_list >>= fun new_files ->
    let files = files @ new_files in
    let occtbl =
      match files with
      | [] -> Odoc_occurrences.Table.v ()
      | file :: files ->
          let strip =
            if strip_path then Odoc_occurrences.Table.strip_table else Fun.id
          in
          let acc = read_occurrences file |> strip in

          List.iter
            (fun file ->
              Odoc_occurrences.aggregate ~tbl:acc
                ~data:(read_occurrences file |> strip))
            files;
          acc
    in
    Io_utils.marshal (Fs.File.to_string dst) occtbl;
    Ok ()
  with Sys_error s -> Error (`Msg s)
