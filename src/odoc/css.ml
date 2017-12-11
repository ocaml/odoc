let copy_default_css ~output_dir =
  let dst =
    let file = Fs.File.create ~directory:output_dir ~name:"odoc.css" in
    Fs.File.to_string file
  in
  let oc = Pervasives.open_out dst in
  Pervasives.output_string oc Css_file.content;
  Pervasives.close_out oc
