open Or_error
open Odoc_utils

let compile_to_json ~output sidebar =
  let json = Odoc_html.Sidebar.to_json sidebar in
  let text = Json.to_string json in
  let output_channel =
    Fs.Directory.mkdir_p (Fs.File.dirname output);
    open_out_bin (Fs.File.to_string output)
  in
  Fun.protect ~finally:(fun () -> close_out output_channel) @@ fun () ->
  Printf.fprintf output_channel "%s" text

let generate ~marshall ~output ~warnings_options:_ ~index =
  Odoc_file.load_index index >>= fun index ->
  let sidebar = Odoc_document.Sidebar.of_lang index in
  match marshall with
  | `JSON -> Ok (compile_to_json ~output sidebar)
  | `Marshall -> Ok (Odoc_file.save_sidebar output sidebar)
