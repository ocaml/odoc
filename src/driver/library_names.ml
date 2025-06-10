open Bos

(** To extract the library names for a given package, without using dune, we

    1. parse the META file of the package with ocamlfind to see which libraries
    exist and what their archive name (.cma filename) is.

    2. use ocamlobjinfo to get a list of all modules within the archives. EDIT:
    it seems this step is now skipped.

    This code assumes that the META file lists for every library an archive
    [archive_name], and that for this cma archive exists a corresponsing
    [archive_name].ocamlobjinfo file. *)

type library = {
  name : string;
  archive_name : string option;
  dir : string option;
  deps : string list;
}

type t = { meta_dir : Fpath.t; libraries : library list }

let read_libraries_from_pkg_defs ~library_name pkg_defs =
  try
    let archive_filename =
      try Some (Fl_metascanner.lookup "archive" [ "byte" ] pkg_defs)
      with _ -> (
        try Some (Fl_metascanner.lookup "archive" [ "native" ] pkg_defs)
        with _ -> None)
    in

    let deps =
      try
        let deps_str = Fl_metascanner.lookup "requires" [] pkg_defs in
        (* The deps_str is a string of space-separated package names, e.g. "a b c" *)
        (* We use Astring to split the string into a list of package names *)
        Astring.String.fields ~empty:false deps_str
      with _ -> []
    in

    let dir =
      List.find_opt (fun d -> d.Fl_metascanner.def_var = "directory") pkg_defs
    in
    let dir = Option.map (fun d -> d.Fl_metascanner.def_value) dir in
    let archive_name =
      Option.bind archive_filename (fun a ->
          let file_name_len = String.length a in
          if file_name_len > 0 then Some (Filename.chop_extension a) else None)
    in
    [ { name = library_name; archive_name; dir; deps } ]
  with Not_found -> []

let process_meta_file file =
  let () = Format.eprintf "process_meta_file: %s\n%!" (Fpath.to_string file) in
  let meta_dir = Fpath.parent file in
  let meta =
    OS.File.with_ic file (fun ic () -> Fl_metascanner.parse ic) ()
    |> Result.get_ok
  in
  let base_library_name =
    if Fpath.basename file = "META" then Fpath.parent file |> Fpath.basename
    else Fpath.get_ext file
  in
  let rec extract_name_and_archive ~prefix
      ((name, pkg_expr) : string * Fl_metascanner.pkg_expr) =
    let library_name = prefix ^ "." ^ name in
    let libraries =
      read_libraries_from_pkg_defs ~library_name pkg_expr.pkg_defs
    in
    let child_libraries =
      pkg_expr.pkg_children
      |> List.map (extract_name_and_archive ~prefix:library_name)
      |> List.flatten
    in
    libraries @ child_libraries
  in
  let libraries =
    read_libraries_from_pkg_defs ~library_name:base_library_name meta.pkg_defs
  in
  let is_not_private (lib : library) =
    not
      (String.split_on_char '.' lib.name
      |> List.exists (fun x -> x = "__private__"))
  in
  let libraries =
    libraries
    @ (meta.pkg_children
      |> List.map (extract_name_and_archive ~prefix:base_library_name)
      |> List.flatten)
    |> List.filter is_not_private
  in
  { meta_dir; libraries }

let libname_of_archive v =
  let { meta_dir; libraries } = v in
  List.fold_left
    (fun acc (x : library) ->
      match x.archive_name with
      | None -> acc
      | Some archive_name ->
          let dir =
            match x.dir with
            | None -> meta_dir
            | Some x -> Fpath.(meta_dir // v x)
          in
          Fpath.Map.update
            Fpath.(dir / archive_name)
            (function
              | None -> Some x.name
              | Some y ->
                  Logs.err (fun m ->
                      m "Multiple libraries for archive %s: %s and %s."
                        archive_name x.name y);
                  Some y)
            acc)
    Fpath.Map.empty libraries

let directories v =
  let { meta_dir; libraries } = v in
  List.fold_left
    (fun acc x ->
      match x.dir with
      | None | Some "" -> Fpath.Set.add meta_dir acc
      | Some x -> (
          let dir = Fpath.(meta_dir // v x) in
          (* NB. topkg installs a META file that points to a ../topkg-care directory
              that is installed by the topkg-care package. We filter that out here,
              though I've not thought of a good way to sort out the `topkg-care` package *)
          match OS.Dir.exists dir with
          | Ok true -> Fpath.Set.add dir acc
          | _ -> acc))
    Fpath.Set.empty libraries
