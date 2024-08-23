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
  archive_name : string;
  dir : string option;
  deps : string list;
}

let read_libraries_from_pkg_defs ~library_name pkg_defs =
  try
    let cma_filename = Fl_metascanner.lookup "archive" [ "byte" ] pkg_defs in
    let deps_str = Fl_metascanner.lookup "requires" [] pkg_defs in
    let deps = Astring.String.fields deps_str in
    let dir =
      List.find_opt (fun d -> d.Fl_metascanner.def_var = "directory") pkg_defs
    in
    let dir = Option.map (fun d -> d.Fl_metascanner.def_value) dir in
    let archive_name =
      let file_name_len = String.length cma_filename in
      if file_name_len > 0 then String.sub cma_filename 0 (file_name_len - 4)
      else cma_filename
    in
    if String.length archive_name > 0 then
      [ { name = library_name; archive_name; dir; deps } ]
    else []
  with Not_found -> []

let process_meta_file file =
  let () = Format.eprintf "process_meta_file: %s\n%!" (Fpath.to_string file) in
  let ic = open_in (Fpath.to_string file) in
  let meta = Fl_metascanner.parse ic in
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
  libraries
