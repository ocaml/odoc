let write ?(without_theme = false) output_dir =
  let file name content =
    let channel =
      Fs.File.create ~directory:output_dir ~name
      |> Fs.File.to_string
      |> Pervasives.open_out
    in
    Pervasives.output_string channel content;
    Pervasives.close_out channel
  in

  if not without_theme then begin
    file "odoc.css" Css_file.content
  end;
  file "highlight.pack.js" Highlight_js.content
