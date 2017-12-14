let read_string = Attrs.read_string

type read_result =
  (Model.Lang.Compilation_unit.t, read_error) result

and read_error =
  | Not_an_interface
  | Wrong_version
  | Corrupted
  | Not_a_typedtree
  | Not_an_implementation

let read_cmti ~make_root ~filename =
  match Cmt_format.read_cmt filename with
  | exception Cmi_format.Error (Not_an_interface _) -> Error Not_an_interface
  | exception Cmt_format.Error (Not_a_typedtree _) -> Error Not_a_typedtree
  | cmt_info ->
    match cmt_info.cmt_annots with
    | Interface intf -> begin
      match cmt_info.cmt_interface_digest with
      | Some digest ->
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
        let unit =
          {Model.Lang.Compilation_unit.id; doc; digest; imports; source;
           interface; hidden; content; expansion = None}
        in
          Ok unit
      | None -> Error Corrupted
    end
    | _ -> Error Not_an_interface

let read_cmt ~make_root ~filename =
  let open Cmi_format in
  let open Cmt_format in
  let open Model.Lang.Compilation_unit in
  try
    let cmt_info = read_cmt filename in
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
          List.map
            (fun file ->
               let pref = Misc.chop_extensions file in
                 String.capitalize_ascii (Filename.basename pref))
            files
        in
        let items = List.sort String.compare items in
        let items =
          List.map
            (fun name ->
               let open Packed in
               let id = Model.Paths.Identifier.Module(id, name) in
               let path = Model.Paths.Path.Root name in
                 {id; path})
            items
        in
        let imports =
          List.filter (fun (name', _) -> name <> name') cmt_info.cmt_imports
        in
        let imports =
          List.map (fun (s, d) -> Import.Unresolved(s, d)) imports
        in
        let doc = Attrs.empty in
        let source = None in
        let content = Pack items in
          Ok {id; doc; digest; imports;
              source; interface; hidden; content; expansion = None}
    | Implementation impl ->
        let open Model.Lang.Compilation_unit in
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
          List.filter (fun (name', _) -> name <> name') cmt_info.cmt_imports
        in
        let imports =
          List.map (fun (s, d) -> Import.Unresolved(s, d)) imports
        in
        let source =
          match cmt_info.cmt_sourcefile, cmt_info.cmt_source_digest with
          | Some file, Some digest ->
            let open Source in
            let build_dir = cmt_info.cmt_builddir in
            Some {file; digest; build_dir}
          | _, _ -> None
        in
        let content = Module items in
        let unit =
          {id; doc; digest; imports;
           source; interface; hidden; content; expansion = None}
        in
          Ok unit
    | _ -> Error Not_an_implementation
  with
  | Cmi_format.Error (Not_an_interface _) -> Error Not_an_implementation
  | Cmi_format.Error (Wrong_version_interface _) -> Error Wrong_version
  | Cmi_format.Error (Corrupted_interface _) -> Error Corrupted
  | Cmt_format.Error (Not_a_typedtree _) -> Error Not_a_typedtree

let read_cmi ~make_root ~filename =
  let open Cmi_format in
  let open Model.Lang.Compilation_unit in
  try
    let cmi_info = read_cmi filename in
      match cmi_info.cmi_crcs with
      | (name, Some digest) :: imports when name = cmi_info.cmi_name ->
          let root = make_root ~module_name:name ~digest:digest in
          let (id, doc, items) =
            Cmi.read_interface root name cmi_info.cmi_sign
          in
          let imports =
            List.map (fun (s, d) -> Import.Unresolved(s, d)) imports
          in
          let interface = true in
          let hidden = Model.Root.contains_double_underscore name in
          let source = None in
          let content = Module items in
          let unit =
            {id; doc; digest; imports;
             source; interface; hidden; content; expansion = None}
          in
            Ok unit
      | _ -> Error Corrupted
  with
  | Cmi_format.Error (Not_an_interface _) -> Error Not_an_interface
  | Cmi_format.Error (Wrong_version_interface _) -> Error Wrong_version
  | Cmi_format.Error (Corrupted_interface _) -> Error Corrupted
