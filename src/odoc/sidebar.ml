open Odoc_model.Lang.Sidebar

let compile ~page_roots ~lib_roots ~output ~warnings_options:_ =
  let resolver =
    Resolver.create ~important_digests:false ~directories:[]
      ~roots:(Some { page_roots; lib_roots; current_root = "" })
      ~open_modules:[]
  in
  let pages =
    List.map
      (fun (page_root, _) ->
        let pages = Resolver.all_pages ~root:page_root resolver in
        let pages =
          List.map
            (fun (page_id, title) ->
              let title =
                match title with
                | None ->
                    [
                      Odoc_model.Location_.at
                        (Odoc_model.Location_.span [])
                        (`Word (Odoc_model.Paths.Identifier.name page_id));
                    ]
                | Some x -> x
              in
              (title, page_id))
            pages
        in
        { page_name = page_root; pages })
      page_roots
  in
  let libraries =
    List.map
      (fun (library, _) ->
        { name = library; units = Resolver.all_units ~library resolver })
      lib_roots
  in
  let content = { pages; libraries } in
  let file = output in
  Fs.Directory.mkdir_p (Fs.File.dirname file);
  let oc = open_out_bin (Fs.File.to_string file) in
  Marshal.to_channel oc content [];
  close_out oc;
  Result.Ok ()

let read input =
  let ic = open_in_bin (Fs.File.to_string input) in
  let content : t = Marshal.from_channel ic in
  close_in ic;
  content
