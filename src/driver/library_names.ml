(** To extract the library names for a given package, without using dune, we

    1. parse the META file of the package with ocamlfind to see which libraries
    exist and what their archive name (.cma filename) is.

    2. use ocamlobjinfo to get a list of all modules within the archives.

    This code assumes that the META file lists for every library an archive
    [archive_name], and that for this cma archive exists a corresponsing
    [archive_name].ocamlobjinfo file. *)

type library = {
  name : string;
  archive_name : string;
  mutable modules : string list;
  dir : string option;
}

type t = { libraries : library list }

let read_libraries_from_pkg_defs ~library_name pkg_defs =
  try
    let cma_filename = Fl_metascanner.lookup "archive" [ "byte" ] pkg_defs in
    let dir = List.find_opt (fun d -> d.Fl_metascanner.def_var = "directory") pkg_defs in
    let dir = Option.map (fun d -> d.Fl_metascanner.def_value) dir in 
    let archive_name =
      let file_name_len = String.length cma_filename in
      if file_name_len > 0 then String.sub cma_filename 0 (file_name_len - 4)
      else cma_filename
    in
    if String.length archive_name > 0 then
      [ { name = library_name; archive_name; modules = []; dir } ]
    else []
  with Not_found -> []

let process_meta_file file =
  let _ = Format.eprintf "process_meta_file: %s\n%!" (Fpath.to_string file) in
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

let process_ocamlobjinfo_file ~(libraries : library list) file =
  let _ =
    Format.eprintf "process_ocamlobjinfo_file: %s\n%!" (Fpath.to_string file)
  in
  let ic = open_in (Fpath.to_string file) in
  let lines = Util.lines_of_channel ic in
  let affix = "Unit name: " in
  let len = String.length affix in
  close_in ic;
  let units =
    List.concat_map
      (fun line ->
        if Astring.String.is_prefix ~affix line then
          [ String.sub line len (String.length line - len) ]
        else [])
      lines
  in
  let _, archive_name = Fpath.split_base file in
  let archive_name = archive_name |> Fpath.rem_ext |> Fpath.to_string in
  let _ =
    Format.eprintf "trying to look up archive_name: %s\nunits: %s\n%!"
      archive_name (String.concat "," units)
  in
  try
    let library =
      List.find (fun l -> l.archive_name = archive_name) libraries
    in
    library.modules <- library.modules @ units
  with Not_found ->
    Format.eprintf "failed to find archive_name: %s\n%!" archive_name;
    ()

(* let get_libraries package =
  let path =  package in
  let maybe_meta_files =
    Bos.OS.Dir.fold_contents ~dotfiles:true
      (fun p acc ->
        let is_meta = p |> Fpath.basename = "META" in
        if is_meta then p :: acc else acc)
      [] path
  in

  match maybe_meta_files with
  | Error (`Msg msg) ->
      failwith
        ("FIXME: error traversing directories to find the META files: " ^ msg)
  | Ok meta_files -> (
      let libraries =
        meta_files |> List.map process_meta_file |> List.flatten
      in

      let _ =
        Format.eprintf "found archive_names: [%s]\n%!"
          (String.concat ", "
             (List.map (fun (l : library) -> l.archive_name) libraries))
      in

      let maybe_ocamlobjinfo_files =
        Bos.OS.Dir.fold_contents ~dotfiles:true
          (fun p acc ->
            let is_ocamlobjinfo = Fpath.get_ext p = ".ocamlobjinfo" in
            if is_ocamlobjinfo then p :: acc else acc)
          [] path
      in
      match maybe_ocamlobjinfo_files with
      | Error (`Msg msg) ->
          failwith
            ("FIXME: error traversing directories to find the ocamlobjinfo \
              files: " ^ msg)
      | Ok ocamlobjinfo_files ->
          List.iter (process_ocamlobjinfo_file ~libraries) ocamlobjinfo_files;
          let _ =
            Format.eprintf "found archive_names: [%s]\n%!"
              (String.concat ", "
                 (List.map
                    (fun (l : library) ->
                      l.archive_name ^ "/" ^ String.concat "," l.modules)
                    libraries))
          in
          { libraries }) *)
