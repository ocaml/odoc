open Bos

type compile_deps = { digest : Digest.t; deps : (string * Digest.t) list }

let odoc = Cmd.v "./_build/default/src/odoc/bin/main.exe"
(* This is the just-built odoc binary *)

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

let compile_deps env f =
  let cmd = Cmd.(odoc % "compile-deps" % Fpath.to_string f) in
  let deps = Run.run env cmd in
  let l = List.filter_map (Astring.String.cut ~sep:" ") deps in
  let basename = Fpath.(basename (f |> rem_ext)) |> String.capitalize_ascii in
  match List.partition (fun (n, _) -> basename = n) l with
  | [ (_, digest) ], deps -> Ok { digest; deps }
  | _ -> Error (`Msg "odd")

let compile env output_dir file includes parent_id =
  let open Cmd in
  let includes =
    Fpath.Set.fold
      (fun path acc -> Cmd.(acc % "-I" % p path))
      includes Cmd.empty
  in
  let cmd =
    odoc % "compile" % Fpath.to_string file % "--output-dir" % p output_dir
    %% includes % "--enable-missing-root-warning"
  in
  let cmd = cmd % "--parent-id" % parent_id in
  let lines = Run.run env cmd in
  add_prefixed_output cmd compile_output (Fpath.to_string file) lines

let compile_impl env output_dir file includes parent_id source_id =
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
  let cmd = cmd % "--parent-id" % parent_id in
  let cmd = cmd % "--source-id" % source_id in
  let lines = Run.run env cmd in
  add_prefixed_output cmd compile_output (Fpath.to_string file) lines

let link env ?(ignore_output = false) file includes =
  let open Cmd in
  let output_file = Fpath.set_ext "odocl" file in
  let includes =
    Fpath.Set.fold
      (fun path acc -> Cmd.(acc % "-I" % p path))
      includes Cmd.empty
  in
  let cmd =
    odoc % "link" % p file % "-o" % p output_file %% includes
    % "--enable-missing-root-warning"
  in
  let cmd =
    if Fpath.to_string file = "stdlib.odoc" then cmd % "--open=\"\"" else cmd
  in
  let lines = Run.run env cmd in
  if not ignore_output then
    add_prefixed_output cmd link_output (Fpath.to_string file) lines

let html_generate env ?(ignore_output = false) ?(assets = [])
    ?(search_uris = []) file source =
  let open Cmd in
  let source =
    match source with None -> empty | Some source -> v "--source" % p source
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
    odoc % "html-generate" %% source % p file %% assets %% search_uris % "-o"
    % "html" % "--theme-uri" % "odoc" % "--support-uri" % "odoc"
  in
  let lines = Run.run env cmd in
  if not ignore_output then
    add_prefixed_output cmd generate_output (Fpath.to_string file) lines

let support_files env =
  let open Cmd in
  let cmd = odoc % "support-files" % "-o" % "html/odoc" in
  Run.run env cmd
let count_occurrences env output =
  let open Cmd in
  let cmd = odoc % "count-occurrences" % "-I" % "." % "-o" % p output in
  Run.run env cmd

let source_tree env ?(ignore_output = false) ~parent ~output file =
  let open Cmd in
  let parent = v "--parent" % ("page-\"" ^ parent ^ "\"") in
  let cmd =
    odoc % "source-tree" % "-I" % "." %% parent % "-o" % p output % p file
  in
  let lines = Run.run env cmd in
  if not ignore_output then
    add_prefixed_output cmd source_tree_output (Fpath.to_string file) lines

let classify env dir =
  let open Cmd in
  let cmd = odoc % "classify" % p dir in
  let lines = Run.run env cmd |> List.filter (fun l -> l <> "") in
  List.map
    (fun line ->
      match String.split_on_char ' ' line with
      | name :: modules -> (name, modules)
      | _ -> failwith "bad classify output")
    lines
