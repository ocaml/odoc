let translate_paths =
  let double_backslash = Str.regexp "\\(.+\\)\\\\" in
  let single_backslash = Str.regexp "\\(.+\\)\\" in
  fun line ->
    line
    |> Str.global_replace double_backslash "\\1/"
    |> Str.global_replace single_backslash "\\1/"

let () =
  let rec loop () =
    match Stdlib.read_line () with
    | line ->
        let line = translate_paths line in
        print_endline line;
        loop ()
    | exception End_of_file -> ()
  in
  loop ()
