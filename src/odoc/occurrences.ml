open Odoc_utils
open Or_error

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

let count ~dst ~warnings_options:_ ~revision directories include_hidden =
  let htbl = Odoc_occurrences.Table.v () in
  let deftbl = Odoc_occurrences.Table.Deftbl.v () in
  let open Odoc_model.Paths.Identifier in
  let get_filepath (id: SourceLocation.t) =
    let src_page = match id.iv with
    | `SourceLocationMod src_page
    | `SourceLocation (src_page, _)
    | `SourceLocationInternal (src_page, _) -> src_page
    in match src_page.iv with
    | `SourcePage (containerpage, filename) ->
      let rec get_containerpage_dir containerpage acc =
        let `Page (parent, name) = (containerpage) in
        match parent with
        | None -> (Odoc_model.Names.PageName.to_string name)::acc
        | Some parent -> get_containerpage_dir parent.iv ((Odoc_model.Names.PageName.to_string name) :: acc)
      in
      (get_containerpage_dir (containerpage.iv) [filename])
  in
  let agg_deftbl () (unit : Odoc_model.Lang.Implementation.t) =
    let () =
      List.iter
        (fun srcinfo ->
          match srcinfo with
          | (Odoc_model.Lang.Source_info.Definition srcloc, (loc : Odoc_model.Lang.Source_info.location_in_file)) ->
              Odoc_occurrences.Table.Deftbl.add deftbl srcloc {
                filepath = get_filepath srcloc
              ; line_number = loc.loc_start.pos_lnum
              };
          | _ -> ()
        ) unit.source_info
    in ()
  in
  let f () (unit : Odoc_model.Lang.Implementation.t) =
    Odoc_occurrences.of_impl ~include_hidden unit htbl deftbl
  in
  fold_dirs ~dirs:directories ~f:agg_deftbl ~init:() >>= fun () ->
  fold_dirs ~dirs:directories ~f ~init:() >>= fun () ->
  let t = Odoc_occurrences.from_occtbl htbl revision in
  Odoc_occurrences.to_file t dst;
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

let read_occurrences file : Odoc_occurrences.Table.t =
  (Odoc_occurrences.from_file file).table

let aggregate files file_list ~warnings_options:_ ~dst =
  try
    parse_input_files file_list >>= fun new_files ->
    let files = files @ new_files in
    let occtbl =
      let acc : Odoc_occurrences.t =
        {table = Odoc_occurrences.Table.v (); revision = ""; max_occurrences = 0}
      in
      List.iter
        (fun file -> Odoc_occurrences.Table.merge_into ~src:(Odoc_occurrences.from_file file).table ~dst:acc.table)
        files
      ;
      acc
    in
    Odoc_occurrences.to_file occtbl dst;
    Ok ()
  with Sys_error s -> Error (`Msg s)
