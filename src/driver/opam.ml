open Bos

let opam = Cmd.v "opam"
let switch = ref None
let prefix = ref None

type package = { name : string; version : string }

let pp fmt p = Format.fprintf fmt "%s.%s" p.name p.version

let rec get_switch () =
  match !switch with
  | None ->
      let cur_switch =
        Util.lines_of_process Cmd.(opam % "switch" % "show") |> List.hd
      in
      switch := Some cur_switch;
      get_switch ()
  | Some s -> s

let prefix () =
  match !prefix with
  | Some p -> p
  | None ->
      let p =
        Util.lines_of_process
          Cmd.(opam % "var" % "--switch" % get_switch () % "prefix")
        |> List.hd
      in
      prefix := Some p;
      p

let all_opam_packages =
  let result = ref None in
  fun () ->
    match !result with
    | Some pkgs -> pkgs
    | None -> (
        let prefix = prefix () in
        match
          Bos.OS.Dir.contents Fpath.(v prefix / ".opam-switch" / "packages")
        with
        | Error (`Msg msg) ->
            Logs.err (fun m -> m "Error listing opam packages: %s" msg);
            []
        | Ok contents ->
            let r =
              List.filter_map
                (fun p ->
                  let name = Fpath.basename p in
                  match Astring.String.cut ~sep:"." name with
                  | Some (name, version) -> Some { name; version }
                  | None -> None)
                contents
            in
            result := Some r;
            r)

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

type installed_files = {
  libs : Fpath.set;
  odoc_pages : Fpath.set;
  other_docs : Fpath.set;
}

type package_of_fpath = package Fpath.map

(* Here we use an associative list *)
type fpaths_of_package = (package * installed_files) list

let pkg_to_dir_map () =
  let pkgs = all_opam_packages () in
  let prefix = prefix () in
  let pkg_content =
    List.map
      (fun p ->
        let contents = pkg_contents p in
        let libs =
          List.fold_left
            (fun set fpath ->
              match Fpath.segs fpath with
              | "lib" :: "stublibs" :: _ -> set
              | "lib" :: _ :: _ :: _ when Fpath.has_ext ".cmi" fpath ->
                  Fpath.Set.add
                    Fpath.(v prefix // fpath |> split_base |> fst)
                    set
              | _ -> set)
            Fpath.Set.empty contents
        in
        let odoc_pages, other_docs =
          List.fold_left
            (fun (odoc_pages, others) fpath ->
              match Fpath.segs fpath with
              | "doc" :: _pkg :: "odoc-pages" :: _ ->
                  Logs.debug (fun m -> m "Found odoc page: %a" Fpath.pp fpath);

                  (Fpath.Set.add Fpath.(v prefix // fpath) odoc_pages, others)
              | "doc" :: _pkg :: "odoc-assets" :: _ ->
                  Logs.debug (fun m -> m "Found odoc page: %a" Fpath.pp fpath);

                  (Fpath.Set.add Fpath.(v prefix // fpath) odoc_pages, others)
              | "doc" :: _ ->
                  Logs.debug (fun m -> m "Found other doc: %a" Fpath.pp fpath);
                  (odoc_pages, Fpath.Set.add Fpath.(v prefix // fpath) others)
              | _ -> (odoc_pages, others))
            Fpath.Set.(empty, empty)
            contents
        in
        Logs.debug (fun m ->
            m "Found %d odoc pages, %d other docs"
              (Fpath.Set.cardinal odoc_pages)
              (Fpath.Set.cardinal other_docs));
        (p, { libs; odoc_pages; other_docs }))
      pkgs
  in
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
  (pkg_content, map)
