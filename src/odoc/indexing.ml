open Odoc_utils
open Astring
open Odoc_json_index
open Or_error
open Odoc_model

module H = Odoc_model.Paths.Identifier.Hashtbl.Any
module Id = Odoc_model.Paths.Identifier

let handle_file file ~unit ~page ~occ =
  match Fpath.basename file with
  | s when String.is_prefix ~affix:"index-" s ->
      Odoc_file.load_index file >>= fun { extra (* libs *); _ } ->
      Ok (occ extra)
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

let compile_to_json ~output ~occurrences files =
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
  let _ : bool =
    List.fold_left
      (fun acc file ->
        match
          handle_file
            ~unit:(print (Json_search.unit ?occurrences) acc)
            ~page:(print Json_search.page acc)
            ~occ:(print (Json_search.index ?occurrences) acc)
            file
        with
        | Ok acc -> acc
        | Error (`Msg m) ->
            Error.raise_warning ~non_fatal:true
              (Error.filename_only "%s" m (Fs.File.to_string file));
            acc)
      true files
  in
  Format.fprintf output "]";
  Ok ()

let compile_to_marshall ~output (pages, libs) files =
  let unit u = [ Odoc_index.Skeleton.from_unit u ] in
  let page p = [ Odoc_index.Skeleton.from_page p ] in
  let index i = i in
  let extra =
    List.concat_map
      ~f:(fun file ->
        match handle_file ~unit ~page ~occ:index file with
        | Ok l -> l
        | Error (`Msg m) ->
            Error.raise_warning ~non_fatal:true
              (Error.filename_only "%s" m (Fs.File.to_string file));
            [])
      files
  in
  let content = { Odoc_index.pages; libs; extra } in
  Ok (Odoc_file.save_index output content)

let read_occurrences file =
  let ic = open_in_bin file in
  let htbl : Odoc_occurrences.Table.t = Marshal.from_channel ic in
  htbl

let find_pages_and_units root =
  Fs.Directory.fold_files_rec_result ~ext:".odocl"
    (fun (pages, units) path ->
      Odoc_file.load path >>= fun { Odoc_file.content; warnings = _ } ->
      match content with
      | Page_content p -> Ok (p :: pages, units)
      | Impl_content _ | Asset_content _ -> Ok (pages, units)
      | Unit_content u -> Ok (pages, u :: units))
    ([], []) root

let page_index ~name = function
  | [] -> None
  | pages ->
      let p_hierarchy = Odoc_index.Page_hierarchy.of_list pages in
      Some { Odoc_index.p_name = name; p_hierarchy }

let lib_index ~name = function
  | [] -> None
  | units ->
      let l_hierarchies = List.map Odoc_index.Skeleton.from_unit units in
      Some { Odoc_index.l_name = name; l_hierarchies }

let compile out_format ~output ~warnings_options ~occurrences ~roots
    ~inputs_in_file ~odocls =
  let handle_warnings f =
    let res = Error.catch_warnings f in
    Error.handle_warnings ~warnings_options res |> Result.join
  in
  handle_warnings @@ fun () ->
  parse_input_files inputs_in_file >>= fun files ->
  let files = List.rev_append odocls files in
  let occurrences =
    match occurrences with
    | None -> None
    | Some occurrences -> Some (read_occurrences (Fpath.to_string occurrences))
  in
  let includes_rec = List.map snd roots in
  let files =
    List.rev_append files
      (includes_rec
      |> List.map (fun include_rec ->
             Fs.Directory.fold_files_rec ~ext:"odocl"
               (fun files file -> file :: files)
               [] include_rec)
      |> List.concat)
  in
  match out_format with
  | `JSON -> compile_to_json ~output ~occurrences files
  | `Marshall ->
      let indexes =
        List.fold_left
          (fun (pages_acc, libs_acc) (name, root) ->
            match find_pages_and_units root with
            | Ok (p, m) ->
                let p = Option.to_list @@ page_index ~name p in
                let l = Option.to_list @@ lib_index ~name m in
                (pages_acc @ p, libs_acc @ l)
            | Error _ -> (pages_acc, libs_acc))
          ([], []) roots
      in
      compile_to_marshall ~output indexes files
