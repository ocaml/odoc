(* What is installed: findlib's libraries, and opam's record of which package
   installed which files. Built once per run, and asked by everything else. *)

type lib = {
  name : string;
  dir : Fpath.t;
  archives : string list;
  requires : Util.StringSet.t;
}

type t = {
  libs : lib Util.StringMap.t;
  by_archive : string Fpath.Map.t;
  no_archive : string Fpath.Map.t;
  owner : string option;
  files : (Opam.fpaths_of_package * Opam.package_of_fpath) Lazy.t;
  other : string -> lib option;
}

let query ~get_dir name =
  match get_dir name with
  | Error _ ->
      Logs.debug (fun m -> m "No directory for library %s" name);
      None
  | Ok dir ->
      let requires =
        match Ocamlfind.direct_deps name with
        | Ok deps -> deps
        | Error (`Msg msg) ->
            Logs.debug (fun m ->
                m "No META dependencies for library '%s': %s" name msg);
            (* Whatever else it needs, it was compiled against this. *)
            Util.StringSet.singleton "stdlib"
      in
      let archives =
        List.map Filename.chop_extension (Ocamlfind.archives name)
        |> List.sort_uniq String.compare
      in
      Some { name; dir; archives; requires }

(* Two libraries can claim one archive, since it is named without its
   extension: compiler-libs.toplevel has ocamltoplevel.cma and
   compiler-libs.native-toplevel ocamltoplevel.cmxa. Its modules go to the
   library with the bytecode archive, or else to the name that sorts first, so
   that the choice does not depend on the order the libraries are listed in. *)
let prefer ~archive a b =
  let byte l = List.mem (archive ^ ".cma") (Ocamlfind.archives l) in
  match (byte a, byte b) with
  | true, false -> a
  | false, true -> b
  | _ -> min a b

let make ~get_dir ~owner names =
  let libs =
    List.fold_left
      (fun acc name ->
        match query ~get_dir name with
        | Some l -> Util.StringMap.add name l acc
        | None -> acc)
      Util.StringMap.empty names
  in
  let by_archive, no_archive =
    Util.StringMap.fold
      (fun _ l (by_archive, no_archive) ->
        match l.archives with
        | [] -> (by_archive, Fpath.Map.add l.dir l.name no_archive)
        | [ archive ] ->
            ( Fpath.Map.update
                Fpath.(l.dir / archive)
                (function
                  | None -> Some l.name
                  | Some x ->
                      let pick = prefer ~archive x l.name in
                      Logs.info (fun m ->
                          m
                            "Libraries %s and %s both have archive %s: picking \
                             %s"
                            x l.name archive pick);
                      Some pick)
                by_archive,
              no_archive )
        | xs ->
            Logs.err (fun m ->
                m "Library %s has several archives: [%a]" l.name
                  Fmt.(list ~sep:sp string)
                  xs);
            (by_archive, no_archive))
      libs
      (Fpath.Map.empty, Fpath.Map.empty)
  in
  {
    libs;
    by_archive;
    no_archive;
    owner;
    files = lazy (Opam.pkg_to_dir_map ());
    other =
      Util.memo ~key:Fun.id (fun _ name ->
          query ~get_dir:Ocamlfind.get_dir name);
  }

let of_switch () =
  make ~get_dir:Ocamlfind.get_dir ~owner:None
    Util.StringSet.(elements (add "stdlib" (of_list (Ocamlfind.all ()))))

let of_prep ~pkg_path ~pkg_name ~libs =
  (* Findlib may place one of the package's libraries outside the prepared
     package: [directory = "+"] means the compiler's library directory, which
     it takes to be the installed one. Such a directory is mapped to the
     package's copy of it, found at the same place below the package. *)
  let roots = Opam.install_roots () in
  let get_dir lib =
    Result.map
      (fun dir ->
        if Fpath.is_prefix pkg_path dir then dir
        else
          match List.find_map (fun r -> Fpath.rem_prefix r dir) roots with
          | Some rel when Bos.OS.Dir.exists Fpath.(pkg_path // rel) = Ok true ->
              Fpath.(to_dir_path (pkg_path // rel))
          | _ -> dir)
      (Ocamlfind.get_dir lib)
  in
  make ~get_dir ~owner:(Some pkg_name) libs

let libs t = List.map snd (Util.StringMap.bindings t.libs)

let find t name =
  match Util.StringMap.find_opt name t.libs with
  | Some l -> Some l
  | None -> t.other name

let requires t name =
  match find t name with
  | Some l -> l.requires
  | None -> Util.StringSet.singleton "stdlib"

(* Findlib does not know which package installed a library; opam's record of
   installed files does, and ocaml-docs-ci keeps it for every package in a
   build's stack. *)
let package t name =
  match (t.owner, Util.StringMap.mem name t.libs) with
  | Some owner, true -> Some owner
  | _ -> (
      match find t name with
      | None -> None
      | Some l ->
          Option.map
            (fun (p : Opam.package) -> p.name)
            (Fpath.Map.find_opt l.dir (snd (Lazy.force t.files))))

let libs_of_package =
  let all = lazy (Ocamlfind.all ()) in
  fun t p -> List.filter (fun l -> package t l = Some p) (Lazy.force all)

let lib_of_archive t path = Fpath.Map.find_opt path t.by_archive
let no_archive_lib t dir = Fpath.Map.find_opt dir t.no_archive
let package_files t = fst (Lazy.force t.files)
