open Bos

let opam = Cmd.v "opam"

type package = { name : string; version : string }

let pp fmt p = Format.fprintf fmt "%s.%s" p.name p.version

let memoize f =
  let r = ref None in
  fun () ->
    match !r with
    | Some x -> x
    | None ->
        let x = f () in
        r := Some x;
        x

let get_switch =
  memoize @@ fun () ->
  Util.lines_of_process Cmd.(opam % "switch" % "show") |> List.hd

let prefix =
  memoize @@ fun () ->
  Util.lines_of_process
    Cmd.(opam % "var" % "--switch" % get_switch () % "prefix")
  |> List.hd

let all_opam_packages =
  memoize @@ fun () ->
  let prefix = prefix () in
  match Bos.OS.Dir.contents Fpath.(v prefix / ".opam-switch" / "packages") with
  | Error (`Msg msg) ->
      Logs.err (fun m -> m "Error listing opam packages: %s" msg);
      []
  | Ok contents ->
      List.filter_map
        (fun p ->
          let name = Fpath.basename p in
          match Astring.String.cut ~sep:"." name with
          | Some (name, version) -> Some { name; version }
          | None -> None)
        contents

let pkg_contents { name; _ } =
  let prefix = Fpath.v (prefix ()) in
  let changes_file =
    Format.asprintf "%a/.opam-switch/install/%s.changes" Fpath.pp prefix name
  in
  let file = OpamFilename.raw changes_file in
  let filename =
    OpamFile.make @@ OpamFilename.raw @@ Filename.basename changes_file
  in
  let changed =
    try
      OpamFilename.with_contents
        (fun str ->
          OpamFile.Changes.read_from_string ~filename
          @@
          (* Field [opam-version] is invalid in [*.changes] files, displaying a warning. *)
          if OpamStd.String.starts_with ~prefix:"opam-version" str then
            match OpamStd.String.cut_at str '\n' with
            | Some (_, str) -> str
            | None -> assert false
          else str)
        file
    with
    | OpamSystem.File_not_found s ->
        Logs.err (fun m ->
            m "File not found: %s.\n%s\nConsidering it empty." changes_file s);
        OpamStd.String.Map.empty
    | OpamPp.Bad_version _ ->
        Logs.err (fun m ->
            m "Bad version while parsing %s.\nConsidering it empty."
              changes_file);
        OpamStd.String.Map.empty
    | OpamPp.Bad_format _ ->
        Logs.err (fun m ->
            m "Bad format while parsing %s.\nConsidering it empty." changes_file);
        OpamStd.String.Map.empty
  in
  let added =
    OpamStd.String.Map.fold
      (fun file x acc ->
        match x with
        | OpamDirTrack.Added _ -> (
            try
              if not @@ Sys.is_directory Fpath.(to_string (prefix // v file))
              then file :: acc
              else acc
            with _ ->
              acc
              (* dose (and maybe others) sometimes creates a symlink to something that doesn't exist *)
            )
        | _ -> acc)
      changed []
  in
  List.map Fpath.v added

let deps pkgs =
  let cmd =
    Cmd.(
      opam % "list" % "--recursive" % "-i" % "--columns" % "package" % "--color"
      % "never" % "-s" % "--or")
  in
  let cmd =
    List.fold_left (fun cmd pkg -> Cmd.(cmd % "--required-by" % pkg)) cmd pkgs
  in
  let out = Util.lines_of_process cmd in
  List.filter_map
    (fun x ->
      match Astring.String.cut ~sep:"." x with
      | Some (name, version) -> Some { name; version }
      | None -> None)
    out

type doc_file = {
  kind : [ `Mld | `Asset | `Other ];
  file : Fpath.t;
  rel_path : Fpath.t;
}

let pp_doc_file fmt { kind; file; rel_path } =
  Format.fprintf fmt "kind: %a@,file: %a@,rel_path: %a@,"
    (Fmt.of_to_string (function
      | `Mld -> "`Mld"
      | `Asset -> "`Asset"
      | `Other -> "`Other"))
    kind Fpath.pp file Fpath.pp rel_path

type installed_files = {
  libs : Fpath.set;
  docs : doc_file list;
  odoc_config : Fpath.t option;
}

type package_of_fpath = package Fpath.map

(* Here we use an associative list *)
type fpaths_of_package = (package * installed_files) list

let pp_fpath_set fmt set =
  Fpath.Set.iter (Format.fprintf fmt "%a@." Fpath.pp) set

let pp_fpaths_of_package fmt l =
  List.iter
    (fun (p, { libs; docs; odoc_config }) ->
      Format.fprintf fmt "%a:@,libs: %a@,docs: %a@,odoc_config: %a@," pp p
        pp_fpath_set libs
        Fmt.Dump.(list pp_doc_file)
        docs (Fmt.option Fpath.pp) odoc_config)
    l

let classify_docs prefix only_package contents =
  let pkg_match pkg =
    match only_package with None -> true | Some p -> p = pkg
  in

  let is_dir f =
    try Sys.is_directory (Fpath.to_string f) with Sys_error _ -> false
  in

  List.fold_left
    (fun acc fpath ->
      match Fpath.segs fpath with
      | "doc" :: pkg :: "odoc-pages" :: _ :: _
        when pkg_match pkg && not (is_dir Fpath.(prefix // fpath)) ->
          Logs.debug (fun m -> m "Found odoc page: %a" Fpath.pp fpath);
          let kind =
            match Fpath.get_ext fpath with ".mld" -> `Mld | _ -> `Asset
          in
          let rel_path =
            Fpath.rem_prefix Fpath.(v "doc" / pkg / "odoc-pages") fpath
            |> Option.get
          in
          { kind; file = Fpath.(prefix // fpath); rel_path } :: acc
      | "doc" :: pkg :: "odoc-assets" :: _ :: _
        when pkg_match pkg && not (is_dir Fpath.(prefix // fpath)) ->
          Logs.debug (fun m -> m "Found odoc page: %a" Fpath.pp fpath);
          let rel_path =
            Fpath.rem_prefix Fpath.(v "doc" / pkg / "odoc-assets") fpath
            |> Option.get
          in
          let rel_path = Fpath.(v "_assets" // rel_path) in
          { kind = `Asset; file = Fpath.(prefix // fpath); rel_path } :: acc
      | [ "doc"; pkg; _ ]
        when pkg_match pkg && not (is_dir Fpath.(prefix // fpath)) ->
          Logs.debug (fun m -> m "Found other doc: %a" Fpath.pp fpath);
          let rel_path = Fpath.base fpath in
          { kind = `Other; file = Fpath.(prefix // fpath); rel_path } :: acc
      | _ -> acc)
    [] contents

let classify_libs prefix only_package contents =
  let pkg_match pkg =
    match only_package with None -> true | Some p -> p = pkg
  in

  let libs =
    List.fold_left
      (fun set fpath ->
        match Fpath.segs fpath with
        | "lib" :: "stublibs" :: _ -> set
        | "lib" :: pkg :: _ :: _
          when Fpath.has_ext ".cmi" fpath && pkg_match pkg ->
            Fpath.Set.add Fpath.(prefix // fpath |> split_base |> fst) set
        | _ -> set)
      Fpath.Set.empty contents
  in
  libs

let find_odoc_config prefix only_package contents =
  let pkg_match pkg =
    match only_package with None -> true | Some p -> p = pkg
  in

  let opt =
    List.find_opt
      (fun fpath ->
        match Fpath.segs fpath with
        | [ "doc"; pkg; "odoc-config.sexp" ] -> pkg_match pkg
        | _ -> false)
      contents
  in

  Option.map (fun p -> Fpath.(prefix // p)) opt

let dune_overrides () =
  let ocamlpath = Sys.getenv_opt "OCAMLPATH" in
  match ocamlpath with
  | None -> []
  | Some path -> (
      (* OCAMLPATH is set in dune to be e.g. /Users/jon/odoc/_build/install/default/lib *)
      (* Let's strip the 'lib' off and we can find the installed files *)
      let path = Fpath.v path in
      match Fpath.segs path |> List.rev with
      | "lib" :: _ :: "install" :: "_build" :: _ -> (
          (* Check it's of the right form *)
          let base = Fpath.split_base path |> fst in
          let contents =
            Bos.OS.Dir.fold_contents
              (fun x acc ->
                match Fpath.relativize ~root:base x with
                | None -> acc
                | Some r -> r :: acc)
              [] base
          in
          match contents with
          | Ok contents ->
              Logs.debug (fun m ->
                  m "dune install contents: %a"
                    Fmt.(Dump.list Fpath.pp)
                    contents);
              let packages =
                List.fold_left
                  (fun acc fpath ->
                    match Fpath.segs fpath with
                    | "lib" :: pkg :: _ :: _ -> Util.StringSet.add pkg acc
                    | "doc" :: pkg :: _ :: _ -> Util.StringSet.add pkg acc
                    | _ -> acc)
                  Util.StringSet.empty contents
              in

              Logs.debug (fun m ->
                  m "Found packages: %a"
                    Fmt.(Dump.list string)
                    (Util.StringSet.elements packages));
              Util.StringSet.fold
                (fun pkg acc ->
                  let libs = classify_libs base (Some pkg) contents in
                  let docs = classify_docs base (Some pkg) contents in
                  let odoc_config = find_odoc_config base (Some pkg) contents in
                  Logs.debug (fun m ->
                      m "pkg %s Found %d docs" pkg (List.length docs));
                  ({ name = pkg; version = "dev" }, { libs; docs; odoc_config })
                  :: acc)
                packages []
          | Error (`Msg msg) ->
              Logs.err (fun m ->
                  m "Error listing dune install directory: %s" msg);
              [])
      | _ -> [])

let check pkgs =
  let cmd =
    Cmd.(
      opam % "list" % "-i" % "--columns" % "package" % "--color" % "never"
      % "-s")
  in
  let cmd = List.fold_left Cmd.( % ) cmd pkgs in
  let out = Util.lines_of_process cmd in
  let res =
    List.filter_map
      (fun x ->
        match Astring.String.cut ~sep:"." x with
        | Some (name, _version) -> Some name
        | None -> None)
      out
  in
  let missing = Util.StringSet.(diff (of_list pkgs) (of_list res)) in
  let dune_pkgnames =
    Util.StringSet.of_list (List.map (fun (p, _) -> p.name) (dune_overrides ()))
  in
  let missing = Util.StringSet.diff missing dune_pkgnames in
  if Util.StringSet.cardinal missing = 0 then Ok () else Error missing

let pkg_to_dir_map () =
  let dune_overrides = dune_overrides () in
  let pkgs = all_opam_packages () in
  let prefix = prefix () in
  let pkg_content =
    List.map
      (fun p ->
        let contents = pkg_contents p in
        let libs = classify_libs (Fpath.v prefix) None contents in
        let docs = classify_docs (Fpath.v prefix) None contents in
        let odoc_config = find_odoc_config (Fpath.v prefix) None contents in
        (p, { libs; docs; odoc_config }))
      pkgs
  in

  (* Remove anything from opam that is present in the dune overrides *)
  let pkg_content =
    List.filter
      (fun (p, _) ->
        not @@ List.exists (fun (p', _) -> p.name = p'.name) dune_overrides)
      pkg_content
  in

  let pkg_content = pkg_content @ dune_overrides in

  let map =
    List.fold_left
      (fun map (p, { libs; _ }) ->
        Fpath.Set.fold
          (fun dir map ->
            Fpath.Map.update dir
              (function
                | None -> Some p
                | Some x ->
                    Logs.debug (fun m ->
                        m "Multiple packages (%a,%a) found for dir %a" pp x pp p
                          Fpath.pp dir);
                    Some p)
              map)
          libs map)
      Fpath.Map.empty pkg_content
  in
  Logs.debug (fun m -> m "pkg_to_dir_map: %a" pp_fpaths_of_package pkg_content);
  (pkg_content, map)
