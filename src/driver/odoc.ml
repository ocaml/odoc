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

let compile_deps f =
  let cmd = Cmd.(!odoc % "compile-deps" % Fpath.to_string f) in
  let desc = Printf.sprintf "Compile deps for %s" (Fpath.to_string f) in
  let deps = Cmd_outputs.submit None desc cmd None in
  let l = List.filter_map (Astring.String.cut ~sep:" ") deps in
  let basename = Fpath.(basename (f |> rem_ext)) |> String.capitalize_ascii in
  match List.partition (fun (n, _) -> basename = n) l with
  | [ (_, digest) ], deps -> Ok { digest; deps }
  | _ -> Error (`Msg "odd")

let compile ~output_dir ~input_file:file ~includes ~warnings_tag ~parent_id
    ~ignore_output =
  let open Cmd in
  let includes =
    Fpath.Set.fold
      (fun path acc -> Cmd.(acc % "-I" % p path))
      includes Cmd.empty
  in

  let output_file =
    let _, f = Fpath.split_base file in
    Some Fpath.(output_dir // Id.to_fpath parent_id // set_ext "odoc" f)
  in
  let cmd =
    !odoc % "compile" % Fpath.to_string file % "--output-dir" % p output_dir
    %% includes % "--enable-missing-root-warning"
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
  ignore @@ Cmd_outputs.submit log desc cmd output_file

let compile_md ~output_dir ~input_file:file ~parent_id =
  let open Cmd in
  let output_file =
    let _, f = Fpath.split_base file in
    Some Fpath.(output_dir // Id.to_fpath parent_id // set_ext "odoc" f)
  in
  let cmd = !odoc_md % p file % "--output-dir" % p output_dir in
  let cmd =
    match Id.to_string parent_id with "" -> cmd | x -> cmd % "--parent-id" % x
  in
  let desc = Printf.sprintf "Compiling Markdown %s" (Fpath.to_string file) in
  let _lines =
    Cmd_outputs.submit
      (Some (`Compile, Fpath.to_string file))
      desc cmd output_file
  in
  ()

let compile_asset ~output_dir ~name ~parent_id =
  let open Cmd in
  let output_file =
    Some
      Fpath.(output_dir // Id.to_fpath parent_id / ("asset-" ^ name ^ ".odoc"))
  in
  let cmd =
    !odoc % "compile-asset" % "--name" % name % "--output-dir" % p output_dir
  in

  let cmd = cmd % "--parent-id" % Id.to_string parent_id in
  let desc = Printf.sprintf "Compiling %s" name in
  ignore @@ Cmd_outputs.submit (Some (`Compile, name)) desc cmd output_file

let compile_impl ~output_dir ~input_file:file ~includes ~parent_id ~source_id =
  let open Cmd in
  let includes =
    Fpath.Set.fold
      (fun path acc -> Cmd.(acc % "-I" % p path))
      includes Cmd.empty
  in
  let cmd =
    !odoc % "compile-impl" % Fpath.to_string file % "--output-dir"
    % p output_dir %% includes % "--enable-missing-root-warning"
  in
  let output_file =
    let _, f = Fpath.split_base file in
    Some
      Fpath.(
        output_dir // Id.to_fpath parent_id
        / ("impl-" ^ to_string (set_ext "odoc" f)))
  in
  let cmd = cmd % "--parent-id" % Id.to_string parent_id in
  let cmd = cmd % "--source-id" % Id.to_string source_id in
  let desc =
    Printf.sprintf "Compiling implementation %s" (Fpath.to_string file)
  in
  ignore
  @@ Cmd_outputs.submit
       (Some (`Compile, Fpath.to_string file))
       desc cmd output_file

let doc_args docs =
  let open Cmd in
  List.fold_left
    (fun acc (pkg_name, path) ->
      let s = Format.asprintf "%s:%a" pkg_name Fpath.pp path in
      v "-P" % s %% acc)
    Cmd.empty docs

let lib_args libs =
  let open Cmd in
  List.fold_left
    (fun acc (libname, path) ->
      let s = Format.asprintf "%s:%a" libname Fpath.pp path in
      v "-L" % s %% acc)
    Cmd.empty libs

let link ?(ignore_output = false) ~custom_layout ~input_file:file ?output_file
    ~docs ~libs ~includes ~warnings_tags ?current_package () =
  let open Cmd in
  let output_file =
    match output_file with Some f -> f | None -> Fpath.set_ext "odocl" file
  in
  let docs = doc_args docs in
  let libs = lib_args libs in
  let includes =
    List.fold_left (fun acc i -> acc % "-I" % p i) empty includes
  in
  let current_package =
    match current_package with
    | None -> Cmd.empty
    | Some c -> Cmd.(v "--current-package" % c)
  in
  let cmd =
    !odoc % "link" % p file % "-o" % p output_file %% docs %% libs %% includes
    %% current_package % "--enable-missing-root-warning"
  in
  let cmd =
    if Fpath.to_string file = "stdlib.odoc" then cmd % "--open=\"\"" else cmd
  in
  let cmd =
    List.fold_left (fun acc k -> acc % "--warnings-tags" % k) cmd warnings_tags
  in
  let desc = Printf.sprintf "Linking %s" (Fpath.to_string file) in
  let cmd = if custom_layout then cmd % "--custom-layout" else cmd in
  let log =
    if ignore_output then None else Some (`Link, Fpath.to_string file)
  in
  ignore @@ Cmd_outputs.submit log desc cmd (Some output_file)

let compile_index ?(ignore_output = false) ~output_file ?occurrence_file ~json
    ~roots ~simplified ~wrap () =
  let roots =
    List.fold_left (fun c r -> Cmd.(c % "--root" % p r)) Cmd.empty roots
  in
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
      % p output_file %% roots %% occ)
  in
  let desc =
    Printf.sprintf "Generating index for %s" (Fpath.to_string output_file)
  in
  let log =
    if ignore_output then None else Some (`Index, Fpath.to_string output_file)
  in
  ignore @@ Cmd_outputs.submit log desc cmd (Some output_file)

let sidebar_generate ?(ignore_output = false) ~output_file ~json input_file () =
  let json = if json then Cmd.v "--json" else Cmd.empty in
  let cmd =
    Cmd.(
      !odoc % "sidebar-generate" %% json %% v "-o" % p output_file
      % p input_file)
  in
  let desc =
    Printf.sprintf "Generating sidebar for %s" (Fpath.to_string output_file)
  in
  let log =
    if ignore_output then None else Some (`Generate, Fpath.to_string output_file)
  in
  ignore @@ Cmd_outputs.submit log desc cmd (Some output_file)

let html_generate ~output_dir ?sidebar ?(ignore_output = false)
    ?(search_uris = []) ?remap ?(as_json = false) ?home_breadcrumb
    ~input_file:file () =
  let open Cmd in
  let index =
    match sidebar with None -> empty | Some idx -> v "--sidebar" % p idx
  in
  let home_breadcrumb =
    match home_breadcrumb with
    | None -> empty
    | Some name -> v "--home-breadcrumb" % name
  in
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
  let log =
    if ignore_output then None else Some (`Generate, Fpath.to_string file)
  in
  ignore @@ Cmd_outputs.submit log desc cmd None

let html_generate_asset ~output_dir ?(ignore_output = false) ?home_breadcrumb
    ~input_file:file ~asset_path () =
  let open Cmd in
  let home_breadcrumb =
    match home_breadcrumb with
    | None -> empty
    | Some name -> v "--home-breadcrumb" % name
  in
  let cmd =
    !odoc % "html-generate-asset" % "-o" % output_dir % "--asset-unit" % p file
    % p asset_path %% home_breadcrumb
  in
  let desc = Printf.sprintf "Copying asset %s" (Fpath.to_string file) in
  let log =
    if ignore_output then None else Some (`Generate, Fpath.to_string file)
  in
  ignore @@ Cmd_outputs.submit log desc cmd None

let html_generate_source ~output_dir ?(ignore_output = false) ~source ?sidebar
    ?(search_uris = []) ?(as_json = false) ?home_breadcrumb ~input_file:file ()
    =
  let open Cmd in
  let file = v "--impl" % p file in
  let sidebar =
    match sidebar with None -> empty | Some idx -> v "--sidebar" % p idx
  in
  let home_breadcrumb =
    match home_breadcrumb with
    | None -> empty
    | Some name -> v "--home-breadcrumb" % name
  in
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
  let log =
    if ignore_output then None else Some (`Generate, Fpath.to_string source)
  in
  ignore @@ Cmd_outputs.submit log desc cmd None

let support_files path =
  let open Cmd in
  let cmd = !odoc % "support-files" % "-o" % Fpath.to_string path in
  let desc = "Generating support files" in
  Cmd_outputs.submit None desc cmd None

let count_occurrences ~input ~output =
  let open Cmd in
  let input = Cmd.of_values Fpath.to_string input in
  let output_c = v "-o" % p output in
  let cmd = !odoc % "count-occurrences" %% input %% output_c in
  let desc = "Counting occurrences" in
  let log = Some (`Count_occurrences, Fpath.to_string output) in
  ignore @@ Cmd_outputs.submit log desc cmd None

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
