let packages root = Sys.readdir root
let versions root dir = Array.to_list @@ Sys.readdir @@ Filename.concat root dir

let untar root =
  Array.iter
    (fun dir ->
      match versions root dir with
      | [] -> ()
      | v :: vs ->
          let latest_version =
            List.fold_left
              (fun v0 v1 ->
                if OpamVersionCompare.compare v0 v1 < 0 then v1 else v0)
              v vs
          in
          Sys.chdir Filename.(concat (concat root dir) latest_version) ;
          let ok = Sys.command "tar -xvf content.tar" in
          assert (ok = 0) ;
          ())
    (packages root)

let contains s1 s2 =
  let re = Str.regexp_string s2 in
  try
    ignore (Str.search_forward re s1 0) ;
    true
  with Not_found -> false

let list root_directory =
  let cwd = Sys.getcwd () in
  Sys.chdir root_directory ;
  let h = Unix.open_process_in "find . -name '*.odocl'" in
  let rec go acc =
    match Stdlib.input_line h with
    | exception End_of_file ->
        ignore (Unix.close_process_in h) ;
        acc
    | line -> go (line :: acc)
  in
  let files = go [] in
  Sys.chdir cwd ;
  List.filter (fun filename ->
      not
        (List.exists (contains filename)
           [ "page-"
           ; "__"
           ; "Linalg"
           ; "tezos"
           ; "archetype"
           ; "async"
           ; "kernel"
           ; "camlp4"
           ; "DAGaml"
           ; "Luv"
           ; "ocapic"
           ]))
  @@ files
