open Astring
open Odoc_json_index
open Or_error
open Odoc_model

let handle_file file ~unit ~page =
  Odoc_file.load file >>= fun unit' ->
  match unit' with
  | { Odoc_file.content = Unit_content unit'; _ } when not unit'.hidden ->
      Ok (unit unit')
  | { Odoc_file.content = Page_content page'; _ } -> Ok (page page')
  | _ ->
      Error
        (`Msg
          "Only pages and unit are allowed as input when generating an index")

let parse_input_file input =
  let is_sep = function '\n' | '\r' -> true | _ -> false in
  Fs.File.read input >>= fun content ->
  let files =
    String.fields ~empty:false ~is_sep content |> List.rev_map Fs.File.of_string
  in
  Ok files

let compile ~output ~warnings_options input =
  parse_input_file input >>= fun files ->
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
  let index () =
    List.fold_left
      (fun acc file ->
        match
          handle_file
            ~unit:(print Json_search.unit acc)
            ~page:(print Json_search.page acc)
            file
        with
        | Ok acc -> acc
        | Error (`Msg m) ->
            Error.raise_warning ~non_fatal:true
              (Error.filename_only "%S" m (Fs.File.to_string input));
            acc)
      true files
  in
  let result = Error.catch_warnings index in
  result |> Error.handle_warnings ~warnings_options >>= fun (_ : bool) ->
  Format.fprintf output "]";
  Ok ()
