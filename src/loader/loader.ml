module Error = Model.Error



let read_string parent_definition location text =
  Error.catch (fun () ->
    Attrs.read_string parent_definition location text)



let corrupted : string -> Error.t =
  Error.filename_only "corrupted"

let not_a_typedtree : string -> Error.t =
  Error.filename_only "not a Typedtree"

let not_an_implementation : string -> Error.t =
  Error.filename_only "not an implementation"

let not_an_interface : string -> Error.t =
  Error.filename_only "not an interface"

let wrong_version : string -> Error.t =
  Error.filename_only "wrong OCaml version"



let read_cmti ~make_root ~filename =
  match Cmt_format.read_cmt filename with
  | exception Cmi_format.Error (Not_an_interface _) ->
    Error (not_an_interface filename)
  | exception Cmt_format.Error (Not_a_typedtree _) ->
    Error (not_a_typedtree filename)
  | cmt_info ->
    match cmt_info.cmt_annots with
    | Interface intf ->
      begin match cmt_info.cmt_interface_digest with
      | None -> Error (corrupted filename)
      | Some digest ->
        Error.catch begin fun () ->
          let name = cmt_info.cmt_modname in
          let root = make_root ~module_name:name ~digest in
          let (id, doc, items) = Cmti.read_interface root name intf in
          let imports =
            List.filter (fun (name', _) -> name <> name') cmt_info.cmt_imports
          in
          let imports =
            List.map (fun (s, d) ->
              Model.Lang.Compilation_unit.Import.Unresolved (s, d))
            imports
          in
          let interface = true in
          let hidden = Model.Root.contains_double_underscore name in
          let source =
            match cmt_info.cmt_sourcefile, cmt_info.cmt_source_digest with
            | Some file, Some digest ->
              let build_dir = cmt_info.cmt_builddir in
              Some {Model.Lang.Compilation_unit.Source.file; digest; build_dir}
            | _, _ -> None
          in
          let content = Model.Lang.Compilation_unit.Module items in
          {Model.Lang.Compilation_unit.id; doc; digest; imports; source;
           interface; hidden; content; expansion = None}
        end
      end
    | _ -> Error (not_an_interface filename)

let read_cmt ~make_root ~filename =
  match Cmt_format.read_cmt filename with
  | exception Cmi_format.Error (Not_an_interface _) ->
    Error (not_an_implementation filename)
  | exception Cmi_format.Error (Wrong_version_interface _) ->
    Error (wrong_version filename)
  | exception Cmi_format.Error (Corrupted_interface _) ->
    Error (corrupted filename)
  | exception Cmt_format.Error (Not_a_typedtree _) ->
    Error (not_a_typedtree filename)
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
      let hidden = Model.Root.contains_double_underscore name in
      let root = make_root ~module_name:name ~digest in
      let id = Model.Paths.Identifier.Root(root, name) in
      let items =
        List.map (fun file ->
          let pref = Misc.chop_extensions file in
          String.capitalize_ascii (Filename.basename pref))
          files
      in
      let items = List.sort String.compare items in
      let items =
        List.map (fun name ->
          let id = Model.Paths.Identifier.Module(id, name) in
          let path = Model.Paths.Path.Root name in
          {Model.Lang.Compilation_unit.Packed.id; path})
          items
      in
      let imports =
        List.filter (fun (name', _) -> name <> name') cmt_info.cmt_imports in
      let imports =
        List.map (fun (s, d) ->
          Model.Lang.Compilation_unit.Import.Unresolved(s, d)) imports
      in
      let doc = Attrs.empty in
      let source = None in
      let content = Model.Lang.Compilation_unit.Pack items in
      Ok {Model.Lang.Compilation_unit.id; doc; digest; imports;
          source; interface; hidden; content; expansion = None}

    | Implementation impl ->
      Error.catch begin fun () ->
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
        let hidden = Model.Root.contains_double_underscore name in
        let root = make_root ~module_name:name ~digest in
        let (id, doc, items) = Cmt.read_implementation root name impl in
        let imports =
          List.filter (fun (name', _) -> name <> name') cmt_info.cmt_imports in
        let imports =
          List.map (fun (s, d) ->
            Model.Lang.Compilation_unit.Import.Unresolved(s, d)) imports
        in
        let source =
          match cmt_info.cmt_sourcefile, cmt_info.cmt_source_digest with
          | Some file, Some digest ->
            let build_dir = cmt_info.cmt_builddir in
            Some {Model.Lang.Compilation_unit.Source.file; digest; build_dir}
          | _, _ -> None
        in
        let content = Model.Lang.Compilation_unit.Module items in
        {Model.Lang.Compilation_unit.id; doc; digest; imports;
         source; interface; hidden; content; expansion = None}
      end

    | _ -> Error (not_an_implementation filename)

let read_cmi ~make_root ~filename =
  match Cmi_format.read_cmi filename with
  | exception Cmi_format.Error (Not_an_interface _) ->
    Error (not_an_interface filename)
  | exception Cmi_format.Error (Wrong_version_interface _) ->
    Error (wrong_version filename)
  | exception Cmi_format.Error (Corrupted_interface _) ->
    Error (corrupted filename)
  | cmi_info ->
    match cmi_info.cmi_crcs with
    | (name, Some digest) :: imports when name = cmi_info.cmi_name ->
      Error.catch begin fun () ->
        let root = make_root ~module_name:name ~digest:digest in
        let (id, doc, items) = Cmi.read_interface root name cmi_info.cmi_sign in
        let imports =
          List.map (fun (s, d) ->
            Model.Lang.Compilation_unit.Import.Unresolved(s, d)) imports
        in
        let interface = true in
        let hidden = Model.Root.contains_double_underscore name in
        let source = None in
        let content = Model.Lang.Compilation_unit.Module items in
        {Model.Lang.Compilation_unit.id; doc; digest; imports;
         source; interface; hidden; content; expansion = None}
      end

    | _ -> Error (corrupted filename)
