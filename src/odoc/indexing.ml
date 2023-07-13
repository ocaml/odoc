open Odoc_search
open Or_error

let map_result f = function Ok v -> Ok (f v) | Error _ as e -> e

let handle_file file ~unit ~page =
  Odoc_file.load file
  |> map_result @@ fun unit' ->
     match unit' with
     | { Odoc_file.content = Unit_content unit'; _ } when not unit'.hidden ->
         Some (unit unit')
     | { Odoc_file.content = Page_content page'; _ } -> Some (page page')
     | _ -> None

let fold_dirs ~dirs ~unit ~page ~init =
  dirs
  |> List.fold_left
       (fun acc dir ->
         acc >>= fun acc ->
         Fs.Directory.fold_files_rec_result ~ext:"odocl"
           (fun acc file ->
             file |> handle_file ~unit:(unit acc) ~page:(page acc) >>= function
             | None -> Ok acc
             | Some acc -> Ok acc)
           acc dir)
       (Ok init)

let compile ~resolver:_ ~parent:_ ~output ~warnings_options:_ dirs =
  let output_channel =
    Fs.Directory.mkdir_p (Fs.File.dirname output);
    open_out_bin (Fs.File.to_string output)
  in
  let output = Format.formatter_of_out_channel output_channel in
  let print f first up =
    if not first then Format.fprintf output ",";
    f output up;
    false
  in
  Format.fprintf output "[";
  fold_dirs ~dirs ~unit:(print Json_search.unit) ~page:(print Json_search.page)
    ~init:true
  >>= fun _ ->
  Format.fprintf output "]";
  Ok ()
