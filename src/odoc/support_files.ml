let should_include ~without_theme file =
  if without_theme then
    match file with
    | "odoc.css" | "fonts/FiraMono-Regular.woff2"
    | "fonts/FiraSans-Regular.woff2" | "fonts/NoticiaText-Regular.ttf" ->
        false
    | _ -> true
  else true

let iter_files ~write ~copy ?(without_theme = false) ~search_files
    output_directory =
  let file name content =
    let name = Fs.File.create ~directory:output_directory ~name in
    write name content
  in
  let files = Odoc_html_support_files.file_list in
  List.iter
    (fun f ->
      match Odoc_html_support_files.read f with
      | Some content when should_include ~without_theme f -> file f content
      | _ -> ())
    files;
  List.iter
    (fun filepath ->
      let origin = filepath in
      let destination =
        Fs.File.create ~directory:output_directory
          ~name:(Fpath.filename filepath)
      in
      copy ~origin ~destination)
    search_files

let write =
  iter_files
    ~write:(fun name content ->
      let dir = Fs.File.dirname name in
      Fs.Directory.mkdir_p dir;
      let name = Fs.File.to_string name in
      let channel = open_out name in
      output_string channel content;
      close_out channel)
    ~copy:(fun ~origin ~destination ->
      let dir = Fs.File.dirname destination in
      Fs.Directory.mkdir_p dir;
      let destination = Fs.File.to_string destination
      and origin = Fs.File.to_string origin in
      let oc = open_out destination and ic = open_in origin in
      try
        while true do
          output_string oc (input_line ic ^ "\n")
        done
      with End_of_file ->
        close_in ic;
        close_out oc)

let print_filenames =
  iter_files
    ~write:(fun name _content -> print_endline (Fs.File.to_string name))
    ~copy:(fun ~origin:_ ~destination ->
      print_endline (Fs.File.to_string destination))
