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
      Odoc_file.load_index file >>= fun index -> Ok (occ index)
  | _ -> (
      Odoc_file.load file >>= fun unit' ->
      match unit' with
      | { Odoc_file.content = Unit_content unit'; _ } when unit'.hidden ->
          Error (`Msg "Hidden units are ignored when generating an index")
      | { Odoc_file.content = Unit_content unit'; _ } -> Ok (unit unit')
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

let compile_to_json ~output ~occurrences hierarchies =
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
  let _first =
    List.fold_left
      (fun first hierarchy ->
        Tree.fold_left
          ~f:(fun first entry ->
            print (Json_search.of_entry ?occurrences) first entry)
          first hierarchy)
      true hierarchies
  in
  Format.fprintf output "]";
  Ok ()

let read_occurrences file =
  let ic = open_in_bin file in
  let htbl : Odoc_occurrences.Table.t = Marshal.from_channel ic in
  htbl

let absolute_normalization p =
  let p =
    if Fpath.is_rel p then Fpath.( // ) (Fpath.v (Sys.getcwd ())) p else p
  in
  Fpath.normalize p

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
  let all_files =
    roots
    |> List.fold_left
         (fun set include_rec ->
           Fs.Directory.fold_files_rec ~ext:"odocl"
             (fun files file ->
               Fpath.Set.add (absolute_normalization file) files)
             set include_rec)
         Fpath.Set.empty
    |> Fpath.Set.to_list
  in
  (* let () = List.iter (Format.printf "%a\n" Fpath.pp) all_files in *)
  let roots = List.map Fs.Directory.to_fpath roots in
  let roots = List.map absolute_normalization roots in
  (* Add an index to keep the original order *)
  let roots = List.mapi (fun i c -> (i, c)) roots in
  let roots =
    (* Make sure that we treat first the "deepest" one *)
    List.sort
      (fun (_, p1) (_, p2) -> if Fpath.is_prefix p1 p2 then 1 else -1)
      roots
  in
  let groups, _ =
    List.fold_left
      (fun (acc, remaining_files) (i, root) ->
        let root_files, remaining_files =
          List.partition (Fpath.is_prefix root) remaining_files
        in
        ((i, root_files) :: acc, remaining_files))
      ([], all_files) roots
  in
  let root_groups =
    List.sort (fun (i, _) (j, _) -> Int.compare i j) groups |> List.map snd
  in
  let root_groups =
    match files with _ :: _ -> files :: root_groups | [] -> root_groups
  in
  let hierarchy_of_group g =
    let pages, modules =
      let read (pages, modules) f =
        match Odoc_file.load f with
        | Ok { content = Page_content p; _ } -> (p :: pages, modules)
        | Ok { content = Unit_content m; _ } -> (pages, m :: modules)
        | _ -> (pages, modules)
      in
      List.fold_left read ([], []) g
    in
    Odoc_index.Skeleton_of.lang ~pages ~modules
  in
  let hierarchies = List.map hierarchy_of_group root_groups in
  match out_format with
  | `JSON -> compile_to_json ~output ~occurrences hierarchies
  | `Marshall -> Ok (Odoc_file.save_index output hierarchies)
