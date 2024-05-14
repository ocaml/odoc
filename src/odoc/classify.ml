(* Classify directories in ocamlfind *)

(* Given a directory with cmis, cmas and so on, partition the modules between the libraries *)
(* open Bos *)

open Misc
open Cmo_format

module StringSet = Set.Make (String)

let debug = ref false

let log fmt =
  if !debug then Format.printf fmt else Format.ifprintf Format.std_formatter fmt

module Archive = struct
  type name = string

  type t = {
    name : name;
    modules : StringSet.t;
    intf_deps : StringSet.t;
    impl_deps : StringSet.t;
  }
  let empty name =
    {
      name;
      modules = StringSet.empty;
      intf_deps = StringSet.empty;
      impl_deps = StringSet.empty;
    }

  let normalise s =
    {
      s with
      intf_deps = StringSet.diff s.intf_deps s.modules;
      impl_deps = StringSet.diff s.impl_deps s.modules;
    }

  let add_cu lib cu =
    normalise
      {
        lib with
        modules = StringSet.add (Odoc_model.Compat.compunit_name cu.cu_name) lib.modules;
        intf_deps =
          List.fold_left
            (fun deps (cu, _) -> StringSet.add cu deps)
            lib.intf_deps cu.cu_imports;
        impl_deps =
          List.fold_left
            (fun deps id -> StringSet.add id deps)
            lib.impl_deps (Odoc_model.Compat.required_compunit_names cu);
      }

  let add_unit_info lib (unit_info : Cmx_format.unit_infos) =
    normalise
      {
        lib with
        modules = StringSet.add unit_info.ui_name lib.modules;
        intf_deps =
          List.fold_left
            (fun deps (unit_info, _) -> StringSet.add unit_info deps)
            lib.intf_deps unit_info.ui_imports_cmi;
        impl_deps =
          List.fold_left
            (fun deps (name, _) -> StringSet.add name deps)
            lib.impl_deps unit_info.ui_imports_cmx;
      }

  let add_module_by_name lib name =
    normalise { lib with modules = StringSet.add name lib.modules }

  let filter_by_cmis valid_cmis lib =
    {
      lib with
      modules = StringSet.filter (fun m -> List.mem m valid_cmis) lib.modules;
    }

  let has_modules a = StringSet.cardinal a.modules > 0

  let pp ppf lib =
    Fmt.pf ppf "Name: %s@.Modules: %a@.Intf deps: %a@.Impl_deps: %a@." lib.name
      Fmt.(list ~sep:sp string)
      (StringSet.elements lib.modules)
      Fmt.(list ~sep:sp string)
      (StringSet.elements lib.intf_deps)
      Fmt.(list ~sep:sp string)
      (StringSet.elements lib.impl_deps)
end

module Cmi = struct
  let get_deps filename =
    let cmi, _cmt = Cmt_format.read filename in
    match cmi with
    | Some cmi -> List.map fst cmi.Cmi_format.cmi_crcs |> StringSet.of_list
    | None -> StringSet.empty
end

module Deps = struct
  let closure deps =
    let rec inner acc l =
      match l with
      | [] -> acc
      | (x, deps) :: rest ->
          let acc =
            List.map
              (fun (y, ydeps) ->
                if StringSet.mem x ydeps then (y, StringSet.union ydeps deps)
                else (y, ydeps))
              acc
          in
          inner acc rest
    in
    let rec loop acc =
      let acc' = inner acc deps in
      if acc = acc' then acc else loop acc'
    in
    loop deps

  (* Return a dag showing dependencies between archives due to module initialisation order *)
  let impl_deps archives =
    List.map
      (fun l1 ->
        let deps =
          List.filter
            (fun l2 ->
              not
              @@ StringSet.is_empty
                   (StringSet.inter l1.Archive.impl_deps l2.Archive.modules))
            archives
        in
        (l1.name, List.map (fun x -> x.Archive.name) deps |> StringSet.of_list))
      archives
    |> closure
end

let read_cma ic init =
  let toc_pos = input_binary_int ic in
  seek_in ic toc_pos;
  let toc = (input_value ic : library) in
  close_in ic;
  Ok (List.fold_left Archive.add_cu init toc.lib_units)

let read_cmxa ic init =
  let li = (input_value ic : Cmx_format.library_infos) in
  close_in ic;
  Ok (List.fold_left Archive.add_unit_info init (List.map fst li.lib_units))

let read_library ic init =
  let open Magic_number in
  match read_current_info ~expected_kind:None ic with
  | Ok { kind = Cma; version = _ } -> read_cma ic init
  | Ok { kind = Cmxa _; version = _ } -> read_cmxa ic init
  | Ok { kind = _; version = _ } -> Error (`Msg "Not a valid library")
  | Error _ -> Error (`Msg "Not a valid file")

let read_cmi ic =
  let open Magic_number in
  match read_current_info ~expected_kind:None ic with
  | Ok { kind = Cmi; version = _ } ->
      let cmi = (input_value ic : Cmi_format.cmi_infos) in
      close_in ic;
      Ok cmi
  | Ok { kind = _; version = _ } -> Error (`Msg "Not a valid cmi")
  | Error _ -> Error (`Msg "Not a valid file")

let classify dir files libraries =
  let libraries = StringSet.elements libraries in

  let archives =
    List.map
      (fun lname ->
        let path ext = Fpath.(v dir / lname |> set_ext ext |> to_string) in
        let paths = [ path ".cma"; path ".cmxa" ] in
        List.fold_left
          (fun cur path ->
            if not (Sys.file_exists path) then cur
            else
              let ic = open_in_bin path in
              match read_library ic cur with
              | Ok lib -> lib
              | Error (`Msg m) ->
                  Format.eprintf "Error reading library: %s\n%!" m;
                  cur)
          (Archive.empty lname) paths)
      libraries
  in

  let cmis = List.filter (fun f -> Fpath.(has_ext ".cmi" (v f))) files in
  let cmi_names =
    List.map
      (fun f -> Fpath.(rem_ext (v f) |> basename |> String.capitalize_ascii))
      cmis
  in

  let _impls, intfs =
    let check f ext =
      Sys.file_exists Fpath.(v dir / f |> set_ext ext |> to_string)
    in
    List.partition (fun f -> check f ".cmo" || check f "cmx") cmis
  in

  let intfs_deps =
    List.map
      (fun f ->
        let modname =
          Filename.chop_suffix f ".cmi" |> String.capitalize_ascii
        in
        (modname, Cmi.get_deps Fpath.(v dir / f |> to_string)))
      intfs
  in

  let modules = List.map fst intfs_deps in

  let orphaned_modules =
    List.filter
      (fun module_name ->
        not
        @@ List.exists
             (fun lib -> StringSet.mem module_name lib.Archive.modules)
             archives)
      modules
  in

  let libdeps = Deps.impl_deps archives in

  let rec topo_sort l =
    match l with
    | [] -> []
    | _ ->
        let no_deps, rest =
          List.partition (function _, x -> StringSet.is_empty x) l
        in
        let no_dep_names = List.map fst no_deps |> StringSet.of_list in
        let rest =
          List.map (fun (x, deps) -> (x, StringSet.diff deps no_dep_names)) rest
        in
        (StringSet.to_seq no_dep_names |> List.of_seq) @ topo_sort rest
  in

  let all_sorted = topo_sort libdeps in
  let find_lib m =
    log "Checking module: %s\n%!" m;

    (* If our module depends on a library, it shouldn't be in any dependency of that library *)
    log "Modules dependencies: %a\n%!"
      Fmt.(list ~sep:sp string)
      (List.assoc m intfs_deps |> StringSet.to_seq |> List.of_seq);
    let denylist =
      List.fold_left
        (fun acc archive ->
          let lib_dependent_modules =
            StringSet.inter (List.assoc m intfs_deps) archive.Archive.modules
          in
          if StringSet.cardinal lib_dependent_modules > 0 then (
            log "Module %s has dependencies [%a] in archive %s\n%!" m
              Fmt.(list ~sep:sp string)
              (StringSet.to_seq lib_dependent_modules |> List.of_seq)
              archive.Archive.name;
            log "Therefore denying: %a\n%!"
              Fmt.(list ~sep:sp string)
              (List.assoc archive.name libdeps
              |> StringSet.to_seq |> List.of_seq);
            StringSet.union acc (List.assoc archive.name libdeps))
          else acc)
        StringSet.empty archives
    in

    log "Denylist: %a\n%!"
      Fmt.(list ~sep:sp string)
      (StringSet.elements denylist);

    (* If library x depends upon our module, our module can't be in any library that depends upon x *)
    let denylist2 =
      List.fold_left
        (fun acc archive ->
          if StringSet.mem m archive.Archive.intf_deps then (
            log "Archive %s is dependent on interface of module %s\n%!"
              archive.Archive.name m;
            List.fold_left
              (fun acc (x, deps) ->
                if StringSet.mem archive.name deps then (
                  log "archive %s depends on archive %s so removing it!\n%!" x
                    archive.name;
                  StringSet.add x acc)
                else acc)
              acc libdeps)
          else acc)
        StringSet.empty archives
    in
    log "Denylist2: %a\n%!"
      Fmt.(list ~sep:sp string)
      (StringSet.elements denylist2);

    (* We prefer to put the module into a library that depends upon our module *)
    let goodlist =
      List.fold_left
        (fun acc archive ->
          if StringSet.mem m archive.Archive.intf_deps then
            StringSet.add archive.name acc
          else acc)
        StringSet.empty archives
    in
    log "Goodlist: %a\n%!"
      Fmt.(list ~sep:sp string)
      (StringSet.elements goodlist);

    let goodlist2 =
      List.fold_left
        (fun acc archive ->
          if
            StringSet.inter archive.Archive.modules (List.assoc m intfs_deps)
            |> StringSet.cardinal > 0
          then StringSet.add archive.name acc
          else acc)
        StringSet.empty archives
    in

    let goodlist = StringSet.union goodlist goodlist2 in

    log "Goodlist: %a\n%!"
      Fmt.(list ~sep:sp string)
      (StringSet.elements goodlist);

    let possibilities =
      StringSet.of_list (List.map (fun x -> x.Archive.name) archives)
    in
    let possibilities = StringSet.diff possibilities denylist in
    let possibilities = StringSet.diff possibilities denylist2 in

    let possibilities =
      if StringSet.is_empty possibilities then goodlist
        (* This can happen, e.g. if Instruct was an interface only module *)
      else StringSet.inter goodlist possibilities
    in

    log "Possibilities: %a\n%!"
      Fmt.(list ~sep:sp string)
      (StringSet.elements possibilities);

    let result =
      try List.find (fun lib -> StringSet.mem lib possibilities) all_sorted
      with Not_found ->
        log "Defaulting to %s\n%!" (List.hd all_sorted);
        List.hd all_sorted
    in

    List.find (fun a -> a.Archive.name = result) archives
  in

  let module_libs =
    List.map
      (fun modname -> (modname, (find_lib modname).Archive.name))
      orphaned_modules
  in

  List.iter
    (fun a ->
      let archive_all =
        List.fold_left
          (fun a (m, lib) ->
            if lib = a.Archive.name then Archive.add_module_by_name a m else a)
          a module_libs
      in
      let archive = Archive.filter_by_cmis cmi_names archive_all in
      if Archive.has_modules archive then
        Printf.printf "%s %s\n" a.Archive.name
          (archive.Archive.modules |> StringSet.elements |> String.concat " "))
    archives;

  ()

let classify dir =
  let files = Sys.readdir dir |> Array.to_list in

  let libraries =
    List.fold_left
      (fun acc f ->
        let p = Fpath.v f in
        if Fpath.has_ext ".cma" p || Fpath.has_ext ".cmxa" p then
          StringSet.add Fpath.(rem_ext p |> to_string) acc
        else acc)
      StringSet.empty files
  in

  if StringSet.cardinal libraries = 0 then Ok ()
  else Ok (classify dir files libraries)
