let write ~output_dir =
  let file name content =
    let channel =
      Fs.File.create ~directory:output_dir ~name
      |> Fs.File.to_string
      |> Pervasives.open_out
    in
    Pervasives.output_string channel content;
    Pervasives.close_out channel
  in

  file "odoc.css" Css_file.content;
  file "highlight.pack.js" Highlight_js.content
