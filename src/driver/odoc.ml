open Bos

module Id : sig
  type t
  val to_fpath : t -> Fpath.t
  val of_fpath : Fpath.t -> t
  val to_string : t -> string
end = struct
  type t = Fpath.t

  let to_fpath id = id

  let of_fpath id = id |> Fpath.normalize |> Fpath.rem_empty_seg
  (* If an odoc path ends with a [/] everything breaks *)

  let to_string id = match Fpath.to_string id with "." -> "" | v -> v
end

let index_filename = "index.odoc-index"

let sidebar_filename = "sidebar.odoc-sidebar"

type compile_deps = { digest : Digest.t; deps : (string * Digest.t) list }

let odoc = ref (Cmd.v "odoc")

let odoc_md = ref (Cmd.v "odoc-md")

(* [-L name:dir] or [-P name:dir], once for each root. *)
let roots flag roots =
  List.fold_left
    (fun acc (name, dir) ->
      Cmd.(acc % flag % (name ^ ":" ^ Fpath.to_string dir)))
    Cmd.empty roots

let compile_deps f =
  let cmd = Cmd.(!odoc % "compile-deps" % Fpath.to_string f) in
  let desc = Printf.sprintf "Compile deps for %s" (Fpath.to_string f) in
  let deps = Cmd_outputs.submit None desc cmd None in
  let l = List.filter_map (Astring.String.cut ~sep:" ") deps in
  let basename = Fpath.(basename (f |> rem_ext)) |> String.capitalize_ascii in
  match List.partition (fun (n, _) -> basename = n) l with
  | [ (_, digest) ], deps -> Ok { digest; deps }
  | _ -> Error (`Msg "odd")

(* [parent_id] determines the unit's identifier; [output_file] is where the
   [.odoc] file goes, which need not be below [parent_id]. *)
let compile ~output_file ~input_file:file ~libs ~warnings_tag ~parent_id
    ~ignore_output =
  let open Cmd in
  let libs = roots "-L" libs in
  let cmd =
    !odoc % "compile" % Fpath.to_string file % "-o" % p output_file %% libs
    % "--enable-missing-root-warning"
  in
  let cmd = cmd % "--parent-id" % Id.to_string parent_id in
  let cmd =
    match warnings_tag with
    | None -> cmd
    | Some tag -> cmd % "--warnings-tag" % tag
  in
  let desc = Printf.sprintf "Compiling %s" (Fpath.to_string file) in
  let log =
    if ignore_output then None else Some (`Compile, Fpath.to_string file)
  in
  Cmd_outputs.v ?log ~output:output_file ~desc cmd

let compile_md ~output_file ~input_file:file ~parent_id =
  let open Cmd in
  let cmd = !odoc_md % p file % "-o" % p output_file in
  let cmd =
    match Id.to_string parent_id with "" -> cmd | x -> cmd % "--parent-id" % x
  in
  let desc = Printf.sprintf "Compiling Markdown %s" (Fpath.to_string file) in
  let log = Some (`Compile, Fpath.to_string file) in
  Cmd_outputs.v ?log ~output:output_file ~desc cmd

let compile_asset ~output_file ~name ~parent_id =
  let open Cmd in
  let cmd =
    !odoc % "compile-asset" % "--name" % name % "-o" % p output_file
    % "--parent-id" % Id.to_string parent_id
  in
  let desc = Printf.sprintf "Compiling %s" name in
  let log = Some (`Compile, name) in
  Cmd_outputs.v ?log ~output:output_file ~desc cmd

let compile_impl ~output_file ~input_file:file ~libs ~parent_id ~source_id =
  let open Cmd in
  let libs = roots "-L" libs in
  let cmd =
    !odoc % "compile-impl" % Fpath.to_string file % "-o" % p output_file %% libs
    % "--enable-missing-root-warning"
  in
  let cmd = cmd % "--parent-id" % Id.to_string parent_id in
  let cmd = cmd % "--source-id" % Id.to_string source_id in
  let desc =
    Printf.sprintf "Compiling implementation %s" (Fpath.to_string file)
  in
  let log = Some (`Compile, Fpath.to_string file) in
  Cmd_outputs.v ?log ~output:output_file ~desc cmd

let link ~ignore_output ~input_file:file ~output_file ~docs ~libs ~warnings_tags
    ?current_package () =
  let open Cmd in
  let docs = roots "-P" docs in
  let libs = roots "-L" libs in
  let current_package =
    match current_package with
    | None -> Cmd.empty
    | Some c -> Cmd.(v "--current-package" % c)
  in
  let cmd =
    !odoc % "link" % p file % "-o" % p output_file %% docs %% libs
    %% current_package % "--enable-missing-root-warning"
  in
  let cmd =
    List.fold_left (fun acc k -> acc % "--warnings-tags" % k) cmd warnings_tags
  in
  let desc = Printf.sprintf "Linking %s" (Fpath.to_string file) in
  let cmd = cmd % "--custom-layout" in
  let log =
    if ignore_output then None else Some (`Link, Fpath.to_string file)
  in
  Cmd_outputs.v ?log ~output:output_file ~desc cmd

(* [file_list] names a file listing the [.odocl] files to index, one per line.
   Passing the files explicitly (rather than [--root] directories) puts them
   all in one hierarchy, whatever their layout on disk. *)
let compile_index ~output_file ?occurrence_file ~json ~file_list ~simplified
    ~wrap () =
  let inputs = Cmd.(v "--file-list" % p file_list) in
  let json = if json then Cmd.v "--json" else Cmd.empty in
  let simplified =
    if simplified then Cmd.v "--simplified-json" else Cmd.empty
  in
  let wrap = if wrap then Cmd.v "--wrap-json" else Cmd.empty in
  let occ =
    match occurrence_file with
    | None -> Cmd.empty
    | Some f -> Cmd.(v "--occurrences" % p f)
  in
  let cmd =
    Cmd.(
      !odoc % "compile-index" %% json %% simplified %% wrap %% v "-o"
      % p output_file %% inputs %% occ)
  in
  let desc =
    Printf.sprintf "Generating index for %s" (Fpath.to_string output_file)
  in
  let log = Some (`Index, Fpath.to_string output_file) in
  Cmd_outputs.v ?log ~output:output_file ~desc cmd

let sidebar_generate ~output_file ~json input_file () =
  let json = if json then Cmd.v "--json" else Cmd.empty in
  let cmd =
    Cmd.(
      !odoc % "sidebar-generate" %% json %% v "-o" % p output_file
      % p input_file)
  in
  let desc =
    Printf.sprintf "Generating sidebar for %s" (Fpath.to_string output_file)
  in
  let log = Some (`Generate, Fpath.to_string output_file) in
  Cmd_outputs.v ?log ~output:output_file ~desc cmd

let html_generate ~output_dir ?sidebar ?(search_uris = []) ?remap
    ?(as_json = false) ~home_breadcrumb ~input_file:file () =
  let open Cmd in
  let index =
    match sidebar with None -> empty | Some idx -> v "--sidebar" % p idx
  in
  let home_breadcrumb = v "--home-breadcrumb" % home_breadcrumb in
  let search_uris =
    List.fold_left
      (fun acc filename -> acc % "--search-uri" % p filename)
      empty search_uris
  in
  let cmd =
    !odoc % "html-generate" % p file %% index %% search_uris % "-o" % output_dir
    %% home_breadcrumb
  in
  let cmd =
    match remap with None -> cmd | Some f -> cmd % "--remap-file" % p f
  in
  let cmd = if as_json then cmd % "--as-json" else cmd in
  let desc = Printf.sprintf "Generating HTML for %s" (Fpath.to_string file) in
  let log = Some (`Generate, Fpath.to_string file) in
  Cmd_outputs.v ?log ~desc cmd

let html_generate_asset ~output_dir ~home_breadcrumb ~input_file:file
    ~asset_path () =
  let open Cmd in
  let home_breadcrumb = v "--home-breadcrumb" % home_breadcrumb in
  let cmd =
    !odoc % "html-generate-asset" % "-o" % output_dir % "--asset-unit" % p file
    % p asset_path %% home_breadcrumb
  in
  let desc = Printf.sprintf "Copying asset %s" (Fpath.to_string file) in
  let log = Some (`Generate, Fpath.to_string file) in
  Cmd_outputs.v ?log ~desc cmd

let html_generate_source ~output_dir ~source ?sidebar ?(search_uris = [])
    ?(as_json = false) ~home_breadcrumb ~input_file:file () =
  let open Cmd in
  let file = v "--impl" % p file in
  let sidebar =
    match sidebar with None -> empty | Some idx -> v "--sidebar" % p idx
  in
  let home_breadcrumb = v "--home-breadcrumb" % home_breadcrumb in
  let search_uris =
    List.fold_left
      (fun acc filename -> acc % "--search-uri" % p filename)
      empty search_uris
  in
  let cmd =
    !odoc % "html-generate-source" %% file %% sidebar % p source %% search_uris
    % "-o" % output_dir %% home_breadcrumb
  in
  let cmd = if as_json then cmd % "--as-json" else cmd in

  let desc = Printf.sprintf "Generating HTML for %s" (Fpath.to_string source) in
  let log = Some (`Generate, Fpath.to_string source) in
  Cmd_outputs.v ?log ~desc cmd

let support_files path =
  let open Cmd in
  let cmd = !odoc % "support-files" % "-o" % Fpath.to_string path in
  let desc = "Generating support files" in
  Cmd_outputs.v ~desc cmd

let count_occurrences ~input ~output =
  let open Cmd in
  let input = Cmd.of_values Fpath.to_string input in
  let output_c = v "-o" % p output in
  let cmd = !odoc % "count-occurrences" %% input %% output_c in
  let desc = "Counting occurrences" in
  let log = Some (`Count_occurrences, Fpath.to_string output) in
  Cmd_outputs.v ?log ~desc cmd

let classify dirs =
  let open Cmd in
  let cmd = List.fold_left (fun cmd d -> cmd % p d) (!odoc % "classify") dirs in
  let desc =
    Format.asprintf "Classifying [%a]" (Fmt.(list ~sep:sp) Fpath.pp) dirs
  in
  let log =
    Some (`Classify, String.concat "," (List.map Fpath.to_string dirs))
  in
  let lines =
    Cmd_outputs.submit log desc cmd None |> List.filter (fun l -> l <> "")
  in
  List.map
    (fun line ->
      match String.split_on_char ' ' line with
      | name :: modules -> (name, modules)
      | _ -> failwith "bad classify output")
    lines
