open Odoc_utils
open Astring
open Odoc_json_index
open Or_error
open Odoc_model

module H = Odoc_model.Paths.Identifier.Hashtbl.Any

let handle_file file ~unit ~page ~occ =
  match Fpath.basename file with
  | s when String.is_prefix ~affix:"index-" s ->
      Odoc_file.load_index file >>= fun { extra (* libs *); _ } ->
      Ok
        (occ
           (* index *)
           (* libs *)
           extra)
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

module Id = Odoc_model.Paths.Identifier

let pages resolver page_roots =
  List.map
    (fun (page_root, _) ->
      let pages = Resolver.all_pages ~root:page_root resolver in
      let p_hierarchy =
        let page_toc_input =
          (* To create a page toc, we need a list with id, title and children
             order. We generate this list from *)
          let prepare_input (id, title, frontmatter) =
            (* We filter non-leaf pages *)
            match id with
            | { Id.iv = #Id.LeafPage.t_pv; _ } as id ->
                (* We generate a title if needed *)
                let title =
                  match title with
                  | None -> Location_.[ at (span []) (`Word (Id.name id)) ]
                  | Some x -> x
                in
                let children_order = frontmatter.Frontmatter.children_order in
                Some (id, title, children_order)
            | _ -> None
          in
          List.filter_map prepare_input pages
        in
        Odoc_index.Page_hierarchy.of_list page_toc_input
      in
      { Odoc_index.p_name = page_root; p_hierarchy })
    page_roots

let libs resolver lib_roots =
  List.map
    (fun (library, _) ->
      let units = Resolver.all_units ~library resolver in
      let l_hierarchies =
        List.filter_map
          (fun (file, _id) ->
            match file () with
            | Some unit -> Some (Odoc_index.Skeleton.from_unit unit)
            | None -> None)
          units
      in
      { Odoc_index.l_name = library; l_hierarchies })
    lib_roots

let compile out_format ~output ~warnings_options ~occurrences ~lib_roots
    ~page_roots ~inputs_in_file ~odocls =
  let handle_warnings f =
    let res = Error.catch_warnings f in
    Error.handle_warnings ~warnings_options res |> Result.join
  in
  handle_warnings @@ fun () ->
  let current_dir = Fs.File.dirname output in
  parse_input_files inputs_in_file >>= fun files ->
  let files = List.rev_append odocls files in
  let occurrences =
    match occurrences with
    | None -> None
    | Some occurrences -> Some (read_occurrences (Fpath.to_string occurrences))
  in
  (* if files = [] && then Error (`Msg "No .odocl files were included") *)
  (* else *)
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
  match out_format with
  | `JSON -> compile_to_json ~output ~occurrences files
  | `Marshall ->
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
      let pages = pages resolver page_roots in
      let libs = libs resolver lib_roots in
      compile_to_marshall ~output (pages, libs) files
