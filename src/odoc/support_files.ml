let iter_files f ?(without_theme = false) output_directory =
  let file name content =
    let name = Fs.File.create ~directory:output_directory ~name in
    f name content
  in
  if not without_theme then file "odoc.css" Css_file.content;
  let files = Odoc_vendor.file_list in
  List.iter
    (fun f ->
      match Odoc_vendor.read f with
      | Some content -> file f content
      | None -> ())
    files

let write =
  iter_files (fun name content ->
      let dir = Fs.File.dirname name in
      Fs.Directory.mkdir_p dir;
      let name = Fs.File.to_string name in
      let channel = open_out name in
      output_string channel content;
      close_out channel)

let print_filenames =
  iter_files (fun name _content -> print_endline (Fs.File.to_string name))
