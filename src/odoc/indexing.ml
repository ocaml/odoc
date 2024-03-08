open Astring
open Odoc_json_index
open Or_error
open Odoc_model

module H = Odoc_model.Paths.Identifier.Hashtbl.Any

let handle_file file ~unit ~page ~occ =
  match Fpath.basename file with
  | s when String.is_prefix ~affix:"index-" s ->
      Odoc_file.load_index file >>= fun index -> Ok (occ index)
  | _ -> (
      Odoc_file.load file >>= fun unit' ->
      match unit' with
      | { Odoc_file.content = Unit_content unit'; _ } when unit'.hidden ->
          Error (`Msg "Hidden units are ignored when generating an index")
      | { Odoc_file.content = Unit_content unit'; _ }
      (* when not unit'.hidden *) ->
          Ok (unit unit')
      | { Odoc_file.content = Page_content page'; _ } -> Ok (page page')
      | _ ->
          Error
            (`Msg
              "Only pages and unit are allowed as input when generating an \
               index"))

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

let compile_to_json ~output ~warnings_options inputs_in_file inputs =
  parse_input_files inputs_in_file >>= fun files ->
  let files = List.rev_append inputs files in
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
            ~occ:(print Json_search.index acc)
            file
        with
        | Ok acc -> acc
        | Error (`Msg m) ->
            Error.raise_warning ~non_fatal:true
              (Error.filename_only "%s" m (Fs.File.to_string file));
            acc)
      true files
  in
  let result = Error.catch_warnings index in
  result |> Error.handle_warnings ~warnings_options >>= fun (_ : bool) ->
  Format.fprintf output "]";
  Ok ()

let compile_to_marshall ~output ~warnings_options inputs_in_file inputs =
  parse_input_files inputs_in_file >>= fun files ->
  let files = List.rev_append inputs files in
  let final_index = H.create 10 in
  let unit u =
    Odoc_model.Fold.unit
      ~f:(fun () item ->
        let entries =
          Odoc_search.Entry.entries_of_item
            (* (u.Odoc_model.Lang.Compilation_unit.id *)
            (*   :> Odoc_model.Paths.Identifier.t) *)
            item
        in
        List.iter
          (fun entry -> H.add final_index entry.Odoc_search.Entry.id entry)
          entries)
      () u
  in
  let page p =
    Odoc_model.Fold.page
      ~f:(fun () item ->
        let entries =
          Odoc_search.Entry.entries_of_item
            (* (p.Odoc_model.Lang.Page.name :> Odoc_model.Paths.Identifier.t) *)
            item
        in
        List.iter
          (fun entry -> H.add final_index entry.Odoc_search.Entry.id entry)
          entries)
      () p
  in
  let index i = H.iter (H.add final_index) i in
  let index () =
    List.fold_left
      (fun acc file ->
        match handle_file ~unit ~page ~occ:index file with
        | Ok acc -> acc
        | Error (`Msg m) ->
            Error.raise_warning ~non_fatal:true
              (Error.filename_only "%s" m (Fs.File.to_string file));
            acc)
      () files
  in
  let result = Error.catch_warnings index in
  result |> Error.handle_warnings ~warnings_options >>= fun () ->
  Ok (Odoc_file.save_index output final_index)

let compile out_format ~output ~warnings_options inputs_in_file inputs =
  match out_format with
  | `JSON -> compile_to_json ~output ~warnings_options inputs_in_file inputs
  | `Marshall ->
      compile_to_marshall ~output ~warnings_options inputs_in_file inputs
