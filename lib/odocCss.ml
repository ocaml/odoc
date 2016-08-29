module Fs = OdocFs

let copy_file ~src ~dst =
  let buf = Bytes.create 65536 in
  let ic = open_in src in
  let oc = open_out dst in
  let rec aux () =
    let read = input ic buf 0 65536 in
    if read <> 0 then (
      output oc buf 0 read;
      aux ()
    )
  in
  aux ();
  close_in ic;
  close_out oc

let copy_default_css ~etc_dir ~output_dir =
  let src = etc_dir ^ "/odoc.css" in
  let dst =
    let file = Fs.File.create ~directory:output_dir ~name:"odoc.css" in
    Fs.File.to_string file
  in
  copy_file ~src ~dst
