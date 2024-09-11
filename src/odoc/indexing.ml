open Astring
open Odoc_json_index
open Or_error
open Odoc_model

let handle_file file ~unit ~page ~occ =
  match Fpath.basename file with
  | s when String.is_prefix ~affix:"index-" s ->
      Odoc_file.load_index file >>= fun index -> Ok (occ index.entries)
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

let compile_to_json ~output ~warnings_options ~occurrences files =
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
            ~unit:(print (Json_search.unit ?occurrences) acc)
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

let compile_to_marshall ~output ~warnings_options ~pages_short_title sidebar
    files =
  let module H = Paths.Identifier.Hashtbl.Any in
  let final_index = H.create 10 in
  let unit u =
    Fold.unit
      ~f:(fun () item ->
        let entries = Odoc_search.Entry.entries_of_item item in
        List.iter
          (fun entry -> H.add final_index entry.Odoc_search.Entry.id entry)
          entries)
      () u
  in
  let page p =
    Fold.page
      ~f:(fun () item ->
        let entries = Odoc_search.Entry.entries_of_item item in
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
  let index =
    { Lang.Index.sidebar; entries = final_index; pages_short_title }
  in
  Ok (Odoc_file.save_index output index)

let read_occurrences file =
  let ic = open_in_bin file in
  let htbl : Odoc_occurrences.Table.t = Marshal.from_channel ic in
  htbl

open Lang.Sidebar

let compile out_format ~output ~warnings_options ~occurrences ~lib_roots
    ~page_roots ~inputs_in_file ~odocls =
  let current_dir = Fs.File.dirname output in
  parse_input_files inputs_in_file >>= fun files ->
  let files = List.rev_append odocls files in
  let occurrences =
    match occurrences with
    | None -> None
    | Some occurrences -> Some (read_occurrences (Fpath.to_string occurrences))
  in
  let resolver =
    Resolver.create ~important_digests:false ~directories:[]
      ~roots:
        (Some
           {
             page_roots;
             lib_roots;
             current_lib = None;
             current_package = None;
             current_dir;
           })
      ~open_modules:[]
  in
  (* if files = [] && then Error (`Msg "No .odocl files were included") *)
  (* else *)
  let all_pages_of_roots =
    List.map
      (fun (page_root, _) ->
        (page_root, Resolver.all_pages ~root:page_root resolver))
      page_roots
  in
  let pages =
    List.map
      (fun (page_root, pages) ->
        let pages =
          List.map
            (fun (page_id, page_info) ->
              let title =
                match page_info.Root.Odoc_file.title with
                | None ->
                    [
                      Location_.at (Location_.span [])
                        (`Word (Paths.Identifier.name page_id));
                    ]
                | Some x -> x
              in
              (title, page_id))
            pages
        in
        { page_name = page_root; pages })
      all_pages_of_roots
  in
  let libraries =
    List.map
      (fun (library, _) ->
        { name = library; units = Resolver.all_units ~library resolver })
      lib_roots
  in
  let includes_rec =
    List.rev_append (List.map snd page_roots) (List.map snd lib_roots)
  in
  let files =
    List.rev_append files
      (includes_rec
      |> List.map (fun include_rec ->
             Fs.Directory.fold_files_rec ~ext:"odocl"
               (fun files file -> file :: files)
               [] include_rec)
      |> List.concat)
  in
  let pages_short_title =
    let module H = Odoc_model.Paths.Identifier.Hashtbl.Page in
    let dst = H.create 8 in
    List.iter
      (fun (_, pages) ->
        List.iter
          (fun (id, page_info) ->
            match
              Frontmatter.get "short_title" page_info.Root.Odoc_file.frontmatter
            with
            | Some short_title -> H.replace dst id short_title
            | None -> ())
          pages)
      all_pages_of_roots;
    dst
  in
  let content = { pages; libraries } in
  match out_format with
  | `JSON -> compile_to_json ~output ~warnings_options ~occurrences files
  | `Marshall ->
      compile_to_marshall ~output ~warnings_options ~pages_short_title content
        files
