open Bos

type compile_deps = { digest : Digest.t; deps : (string * Digest.t) list }

let odoc = Cmd.v "./_build/default/src/odoc/bin/main.exe"
(* This is the just-built odoc binary *)

let submit desc cmd output_file =
  match Worker_pool.submit desc cmd output_file with
  | Ok x -> x
  | Error exn -> raise exn

let compile_output = ref [ "" ]

let compile_src_output = ref [ "" ]

let link_output = ref [ "" ]

let generate_output = ref [ "" ]

let source_tree_output = ref [ "" ]

let add_prefixed_output cmd list prefix lines =
  if List.length lines > 0 then
    list :=
      !list
      @ (Bos.Cmd.to_string cmd :: List.map (fun l -> prefix ^ ": " ^ l) lines)

let compile_deps f =
  let cmd = Cmd.(odoc % "compile-deps" % Fpath.to_string f) in
  let desc = Printf.sprintf "Compile deps for %s" (Fpath.to_string f) in
  let deps = submit desc cmd None in
  let l = List.filter_map (Astring.String.cut ~sep:" ") deps in
  let basename = Fpath.(basename (f |> rem_ext)) |> String.capitalize_ascii in
  match List.partition (fun (n, _) -> basename = n) l with
  | [ (_, digest) ], deps -> Ok { digest; deps }
  | _ -> Error (`Msg "odd")

let compile ~output_dir ~input_file:file ~includes ~parent_id =
  let open Cmd in
  let includes =
    Fpath.Set.fold
      (fun path acc -> Cmd.(acc % "-I" % p path))
      includes Cmd.empty
  in
  let output_file =
    let _, f = Fpath.split_base file in
    Some Fpath.(output_dir // v parent_id // set_ext "odoc" f)
  in
  let cmd =
    odoc % "compile" % Fpath.to_string file % "--output-dir" % p output_dir
    %% includes % "--enable-missing-root-warning"
  in
  let cmd = cmd % "--parent-id" % parent_id in
  let desc = Printf.sprintf "Compiling %s" (Fpath.to_string file) in
  let lines = submit desc cmd output_file in
  add_prefixed_output cmd compile_output (Fpath.to_string file) lines

let compile_impl ~output_dir ~input_file:file ~includes ~parent_id ~source_id =
  let open Cmd in
  let includes =
    Fpath.Set.fold
      (fun path acc -> Cmd.(acc % "-I" % p path))
      includes Cmd.empty
  in
  let cmd =
    odoc % "compile-impl" % Fpath.to_string file % "--output-dir" % p output_dir
    %% includes % "--enable-missing-root-warning"
  in
  let output_file =
    let _, f = Fpath.split_base file in
    Some
      Fpath.(
        output_dir // v parent_id / ("impl-" ^ to_string (set_ext "odoc" f)))
  in
  let cmd = cmd % "--parent-id" % parent_id in
  let cmd = cmd % "--source-id" % source_id in
  let desc =
    Printf.sprintf "Compiling implementation %s" (Fpath.to_string file)
  in
  let lines = submit desc cmd output_file in
  add_prefixed_output cmd compile_output (Fpath.to_string file) lines

let link ?(ignore_output = false) ~input_file:file ~includes ~docs ~libs () =
  let open Cmd in
  let output_file = Fpath.set_ext "odocl" file in
  let includes =
    Fpath.Set.fold
      (fun path acc -> Cmd.(acc % "-I" % p path))
      includes Cmd.empty
  in
  let docs =
    List.fold_left
      (fun acc (pkgname, path) ->
        let s = Format.asprintf "%s:%a" pkgname Fpath.pp path in
        v "-P" % s %% acc)
      Cmd.empty docs
  in
  let libs =
    List.fold_left
      (fun acc (libname, path) ->
        let s = Format.asprintf "%s:%a" libname Fpath.pp path in
        v "-L" % s %% acc)
      Cmd.empty libs
  in
  let cmd =
    odoc % "link" % p file % "-o" % p output_file %% includes %% docs %% libs
    % "--enable-missing-root-warning"
  in
  let cmd =
    if Fpath.to_string file = "stdlib.odoc" then cmd % "--open=\"\"" else cmd
  in
  let desc = Printf.sprintf "Linking %s" (Fpath.to_string file) in

  let lines = submit desc cmd (Some output_file) in
  if not ignore_output then
    add_prefixed_output cmd link_output (Fpath.to_string file) lines

let sidebar ?(ignore_output = false) ~docs ~libs ~output_file () =
  let open Cmd in
  let docs =
    List.fold_left
      (fun acc (pkgname, path) ->
        let s = Format.asprintf "%s:%a" pkgname Fpath.pp path in
        v "-P" % s %% acc)
      Cmd.empty docs
  in
  let libs =
    List.fold_left
      (fun acc (libname, path) ->
        let s = Format.asprintf "%s:%a" libname Fpath.pp path in
        v "-L" % s %% acc)
      Cmd.empty libs
  in
  let cmd = odoc % "sidebar" % "-o" % p output_file %% docs %% libs in
  let desc = Printf.sprintf "Sidebar for %s" (Fpath.to_string output_file) in

  let lines = submit desc cmd (Some output_file) in
  if not ignore_output then
    add_prefixed_output cmd link_output (Fpath.to_string output_file) lines

let html_generate ~output_dir ?sidebar ?(ignore_output = false) ?(assets = [])
    ?source ?(search_uris = []) ~input_file:file () =
  let open Cmd in
  let source =
    match source with None -> empty | Some source -> v "--source" % p source
  in
  let sidebar =
    match sidebar with None -> empty | Some sb -> v "--sidebar" % p sb
  in
  let assets =
    List.fold_left (fun acc filename -> acc % "--asset" % filename) empty assets
  in
  let search_uris =
    List.fold_left
      (fun acc filename -> acc % "--search-uri" % p filename)
      empty search_uris
  in
  let cmd =
    odoc % "html-generate" %% source % p file %% assets %% sidebar
    %% search_uris % "-o" % output_dir
  in
  let desc = Printf.sprintf "Generating HTML for %s" (Fpath.to_string file) in
  let lines = submit desc cmd None in
  if not ignore_output then
    add_prefixed_output cmd generate_output (Fpath.to_string file) lines

let support_files path =
  let open Cmd in
  let cmd = odoc % "support-files" % "-o" % Fpath.to_string path in
  let desc = "Generating support files" in
  submit desc cmd None

let count_occurrences output =
  let open Cmd in
  let cmd = odoc % "count-occurrences" % "-I" % "." % "-o" % p output in
  let desc = "Counting occurrences" in
  submit desc cmd None

let source_tree ?(ignore_output = false) ~parent ~output file =
  let open Cmd in
  let parent = v "--parent" % ("page-\"" ^ parent ^ "\"") in
  let cmd =
    odoc % "source-tree" % "-I" % "." %% parent % "-o" % p output % p file
  in
  let desc = Printf.sprintf "Source tree for %s" (Fpath.to_string file) in
  let lines = submit desc cmd None in
  if not ignore_output then
    add_prefixed_output cmd source_tree_output (Fpath.to_string file) lines

let classify dir =
  let open Cmd in
  let cmd = odoc % "classify" % p dir in
  let desc = Printf.sprintf "Classifying %s" (Fpath.to_string dir) in
  let lines = submit desc cmd None |> List.filter (fun l -> l <> "") in
  List.map
    (fun line ->
      match String.split_on_char ' ' line with
      | name :: modules -> (name, modules)
      | _ -> failwith "bad classify output")
    lines
