open Odoc_compat
open Result

module Error = Odoc_model.Error



let read_string parent_definition filename text =
  let location =
    let pos =
      Lexing.{
        pos_fname = filename;
        pos_lnum = 0;
        pos_cnum = 0;
        pos_bol = 0
      }
    in
    Location.{ loc_start = pos; loc_end = pos; loc_ghost = true }
  in
  Error.catch_errors_and_warnings (fun () ->
    Doc_attr.page parent_definition location text)



let corrupted file =
  Error.raise_exception (Error.filename_only "corrupted" file)

let not_a_typedtree file =
  Error.raise_exception (Error.filename_only "not a Typedtree" file)

let not_an_implementation file =
  Error.raise_exception (Error.filename_only "not an implementation" file)

let not_an_interface file =
  Error.raise_exception (Error.filename_only "not an interface" file)

let wrong_version file =
  Error.raise_exception (Error.filename_only "wrong OCaml version" file)

let error_msg file (msg : string) =
  Error.raise_exception (Error.filename_only "%s" msg file)



let read_cmti ~make_root ~parent ~filename () =
  match Cmt_format.read_cmt filename with
  | exception Cmi_format.Error (Not_an_interface _) ->
    not_an_interface filename
  | exception Cmt_format.Error (Not_a_typedtree _) ->
    not_a_typedtree filename
  | cmt_info ->
    match cmt_info.cmt_annots with
    | Interface intf ->
      begin match cmt_info.cmt_interface_digest with
      | None -> corrupted filename
      | Some digest ->
        let name = cmt_info.cmt_modname in
        let root =
          match make_root ~module_name:name ~digest with
          | Ok root -> root
          | Error (`Msg m) -> error_msg filename m
        in
        let (id, doc, items) = Cmti.read_interface parent name intf in
        let imports =
          List.filter (fun (name', _) -> name <> name') cmt_info.cmt_imports
        in
        let imports =
          List.map (fun (s, d) ->
            Odoc_model.Lang.Compilation_unit.Import.Unresolved (s, d))
          imports
        in
        let interface = true in
        let hidden = Odoc_model.Root.contains_double_underscore name in
        let source =
          match cmt_info.cmt_sourcefile, cmt_info.cmt_source_digest with
          | Some file, Some digest ->
            let build_dir = cmt_info.cmt_builddir in
            Some {Odoc_model.Lang.Compilation_unit.Source.file; digest; build_dir}
          | _, _ -> None
        in
        let content = Odoc_model.Lang.Compilation_unit.Module items in
        {Odoc_model.Lang.Compilation_unit.id; root; doc; digest; imports; source;
         interface; hidden; content; expansion = None; linked = false}
      end
    | _ -> not_an_interface filename

let read_cmt ~make_root ~parent ~filename () =
  match Cmt_format.read_cmt filename with
  | exception Cmi_format.Error (Not_an_interface _) ->
    not_an_implementation filename
  | exception Cmi_format.Error (Wrong_version_interface _) ->
    wrong_version filename
  | exception Cmi_format.Error (Corrupted_interface _) ->
    corrupted filename
  | exception Cmt_format.Error (Not_a_typedtree _) ->
    not_a_typedtree filename
  | cmt_info ->
    match cmt_info.cmt_annots with
    | Packed(_, files) ->
      let name = cmt_info.cmt_modname in
      let interface, digest =
        match cmt_info.cmt_interface_digest with
        | Some digest -> true, digest
        | None ->
          match List.assoc name cmt_info.cmt_imports with
          | Some digest -> false, digest
          | None -> assert false
          | exception Not_found -> assert false
      in
      let hidden = Odoc_model.Root.contains_double_underscore name in
      let root =
        match make_root ~module_name:name ~digest with
        | Ok root -> root
        | Error (`Msg m) -> error_msg filename m
      in
      let id = `Root(parent, Odoc_model.Names.ModuleName.make_std name) in
      let items =
        List.map (fun file ->
          let pref = Misc.chop_extensions file in
          String.capitalize_ascii (Filename.basename pref))
          files
      in
      let items = List.sort String.compare items in
      let items =
        List.map (fun name ->
          let id = `Module(id, Odoc_model.Names.ModuleName.make_std name) in
          let path = `Root name in
          {Odoc_model.Lang.Compilation_unit.Packed.id; path})
          items
      in
      let imports =
        List.filter (fun (name', _) -> name <> name') cmt_info.cmt_imports in
      let imports =
        List.map (fun (s, d) ->
          Odoc_model.Lang.Compilation_unit.Import.Unresolved(s, d)) imports
      in
      let doc = Doc_attr.empty in
      let source = None in
      let content = Odoc_model.Lang.Compilation_unit.Pack items in
      {Odoc_model.Lang.Compilation_unit.id; root; doc; digest; imports;
          source; interface; hidden; content; expansion = None; linked = false}

    | Implementation impl ->
      let name = cmt_info.cmt_modname in
      let interface, digest =
        match cmt_info.cmt_interface_digest with
        | Some digest -> true, digest
        | None ->
          match List.assoc name cmt_info.cmt_imports with
          | Some digest -> false, digest
          | None -> assert false
          | exception Not_found -> assert false
      in
      let hidden = Odoc_model.Root.contains_double_underscore name in
      let root =
        match make_root ~module_name:name ~digest with
        | Ok root -> root
        | Error (`Msg m) -> error_msg filename m
      in
      let (id, doc, items) = Cmt.read_implementation parent name impl in
      let imports =
        List.filter (fun (name', _) -> name <> name') cmt_info.cmt_imports in
      let imports =
        List.map (fun (s, d) ->
          Odoc_model.Lang.Compilation_unit.Import.Unresolved(s, d)) imports
      in
      let source =
        match cmt_info.cmt_sourcefile, cmt_info.cmt_source_digest with
        | Some file, Some digest ->
          let build_dir = cmt_info.cmt_builddir in
          Some {Odoc_model.Lang.Compilation_unit.Source.file; digest; build_dir}
        | _, _ -> None
      in
      let content = Odoc_model.Lang.Compilation_unit.Module items in
      {Odoc_model.Lang.Compilation_unit.id; root; doc; digest; imports;
       source; interface; hidden; content; expansion = None; linked = false}

    | _ -> not_an_implementation filename

let read_cmi ~make_root ~parent ~filename () =
  match Cmi_format.read_cmi filename with
  | exception Cmi_format.Error (Not_an_interface _) ->
    not_an_interface filename
  | exception Cmi_format.Error (Wrong_version_interface _) ->
    wrong_version filename
  | exception Cmi_format.Error (Corrupted_interface _) ->
    corrupted filename
  | cmi_info ->
    match cmi_info.cmi_crcs with
    | (name, Some digest) :: imports when name = cmi_info.cmi_name ->
      let root =
        match make_root ~module_name:name ~digest with
        | Ok root -> root
        | Error (`Msg m) -> error_msg filename m
      in
      let (id, doc, items) = Cmi.read_interface parent name (Odoc_model.Compat.signature cmi_info.cmi_sign) in
      let imports =
        List.map (fun (s, d) ->
          Odoc_model.Lang.Compilation_unit.Import.Unresolved(s, d)) imports
      in
      let interface = true in
      let hidden = Odoc_model.Root.contains_double_underscore name in
      let source = None in
      let content = Odoc_model.Lang.Compilation_unit.Module items in
      {Odoc_model.Lang.Compilation_unit.id; root; doc; digest; imports;
       source; interface; hidden; content; expansion = None; linked = false}

    | _ -> corrupted filename

let read_cmti ~make_root ~parent ~filename =
  Odoc_model.Error.catch_errors_and_warnings (read_cmti ~make_root ~parent ~filename)

let read_cmt ~make_root ~parent ~filename =
  Odoc_model.Error.catch_errors_and_warnings (read_cmt ~make_root ~parent ~filename)

let read_cmi ~make_root ~parent ~filename =
  Odoc_model.Error.catch_errors_and_warnings (read_cmi ~make_root ~parent ~filename)
