open Result
module Error = Odoc_model.Error

let read_string parent_definition filename text =
  let location =
    let pos =
      Lexing.{ pos_fname = filename; pos_lnum = 1; pos_cnum = 0; pos_bol = 0 }
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

type make_root =
  module_name:string ->
  digest:Digest.t ->
  (Odoc_model.Root.t, [ `Msg of string ]) result

exception Corrupted

exception Not_an_implementation

exception Not_an_interface

exception Make_root_error of string

let read_cmt_infos source_id ~filename root digest imports () =
  match Cmt_format.read_cmt filename with
  | exception Cmi_format.Error _ -> raise Corrupted
  | cmt_info -> (
      match cmt_info.cmt_annots with
      | Implementation impl ->
          let shape_infos =
            Odoc_model.Compat.shape_info_of_cmt_infos cmt_info
          in
          Implementation.read_cmt_infos source_id shape_infos impl digest root
            imports
      | _ -> raise Not_an_implementation)

let make_compilation_unit ~make_root ~imports ~interface ?sourcefile ~name ~id
    ?canonical content =
  let open Odoc_model.Lang.Compilation_unit in
  let interface, digest =
    match interface with
    | Some digest -> (true, digest)
    | None -> (
        match List.assoc name imports with
        | Some digest -> (false, digest)
        | None -> raise Corrupted
        | exception Not_found -> raise Corrupted)
  in
  let root =
    match make_root ~module_name:name ~digest with
    | Ok root -> root
    | Error (`Msg m) -> raise (Make_root_error m)
  in
  let imports = List.filter (fun (name', _) -> name <> name') imports in
  let imports = List.map (fun (s, d) -> Import.Unresolved (s, d)) imports in
  let source =
    match sourcefile with
    | Some (Some file, Some digest, build_dir) ->
        Some { Source.file; digest; build_dir }
    | _ -> None
  in
  {
    id;
    root;
    digest;
    imports;
    source;
    interface;
    hidden = Odoc_model.Names.contains_double_underscore name;
    content;
    expansion = None;
    linked = false;
    canonical;
    source_loc = None;
  }

let compilation_unit_of_sig ~make_root ~imports ~interface ?sourcefile ~name ~id
    ?canonical sg =
  let content = Odoc_model.Lang.Compilation_unit.Module sg in
  make_compilation_unit ~make_root ~imports ~interface ?sourcefile ~name ~id
    ?canonical content

let read_cmti ~make_root ~parent ~filename ~warnings_tag () =
  let cmt_info = Cmt_format.read_cmt filename in
  match cmt_info.cmt_annots with
  | Interface intf -> (
      match cmt_info.cmt_interface_digest with
      | None -> raise Corrupted
      | Some digest as interface ->
          let _ =
            try Odoc_model.Names.set_unique_ident (Digest.to_hex digest)
            with _ -> ()
          in
          let name = cmt_info.cmt_modname |> Compilation_unit.name_as_string in
          let sourcefile =
            ( cmt_info.cmt_sourcefile,
              cmt_info.cmt_source_digest,
              cmt_info.cmt_builddir )
          in
          let id, sg, canonical =
            Cmti.read_interface parent name ~warnings_tag intf
          in
          let imports =
            cmt_info.cmt_imports
            |> Array.map (fun import ->
              Import_info.name import |> Compilation_unit.Name.to_string,
              Import_info.crc import)
            |> Array.to_list
          in
          compilation_unit_of_sig ~make_root ~imports
            ~interface ~sourcefile ~name ~id ?canonical sg)
  | _ -> raise Not_an_interface

let read_cmt ~make_root ~parent ~filename ~warnings_tag () =
  match Cmt_format.read_cmt filename with
  | exception Cmi_format.Error (Not_an_interface _) ->
      raise Not_an_implementation
  | cmt_info -> (
      let name = cmt_info.cmt_modname |> Compilation_unit.name_as_string in
      let sourcefile =
        ( cmt_info.cmt_sourcefile,
          cmt_info.cmt_source_digest,
          cmt_info.cmt_builddir )
      in
      let interface = cmt_info.cmt_interface_digest in
      (match cmt_info.cmt_interface_digest with
      | None -> (
          match cmt_info.cmt_source_digest with
          | Some x -> (
              try Odoc_model.Names.set_unique_ident (Digest.to_hex x)
              with _ -> ())
          | None -> ( try Odoc_model.Names.set_unique_ident name with _ -> ()))
      | Some digest -> (
          try Odoc_model.Names.set_unique_ident (Digest.to_hex digest)
          with _ -> ()));
      let imports =
        cmt_info.cmt_imports
        |> Array.map (fun import ->
             Import_info.name import |> Compilation_unit.Name.to_string,
             Import_info.crc import)
        |> Array.to_list
      in
      match cmt_info.cmt_annots with
      | Packed (_, files) ->
          let id =
            Odoc_model.Paths.Identifier.Mk.root
              (parent, Odoc_model.Names.ModuleName.make_std name)
          in
          let items =
            List.map
              (fun file ->
                let pref = Misc.chop_extensions file in
                Astring.String.Ascii.capitalize (Filename.basename pref))
              files
          in
          let items = List.sort String.compare items in
          let items =
            List.map
              (fun name ->
                let id =
                  Odoc_model.Paths.Identifier.Mk.module_
                    (id, Odoc_model.Names.ModuleName.make_std name)
                in
                let path = `Root (Odoc_model.Names.ModuleName.make_std name) in
                { Odoc_model.Lang.Compilation_unit.Packed.id; path })
              items
          in
          let content = Odoc_model.Lang.Compilation_unit.Pack items in
          make_compilation_unit ~make_root ~imports ~interface ~sourcefile ~name
            ~id content
      | Implementation impl ->
          let id, sg, canonical =
            Cmt.read_implementation parent name ~warnings_tag impl
          in
          compilation_unit_of_sig ~make_root ~imports ~interface ~sourcefile
            ~name ~id ?canonical sg
      | _ -> raise Not_an_implementation)

let compilation_unit_of_import_info (info : Import_info.Intf.Nonalias.t option) =
  match info with
  | None -> None
  | Some (Parameter, _) -> None
  | Some (Normal cu, _) -> Some (cu |> Compilation_unit.full_path_as_string)

let read_cmi ~make_root ~parent ~filename ~warnings_tag () =
  let cmi_info = Cmi_format.read_cmi filename in
  let cmi_crcs =
    List.map (fun import ->
        Import_info.name import, Import_info.Intf.info import)
      (Array.to_list cmi_info.cmi_crcs)
  in
  match cmi_crcs with
  | (name, (Some _ as interface)) :: imports
    when name = cmi_info.cmi_name ->
      let name = name |> Compilation_unit.Name.to_string in
      let id, sg =
        Cmi.read_interface parent name ~warnings_tag
          (Odoc_model.Compat.signature cmi_info.cmi_sign)
      in
      let imports =
        imports
        |> List.map (fun (name, info_opt) ->
             name |> Compilation_unit.Name.to_string,
             compilation_unit_of_import_info info_opt)
      in
      let interface = interface |> Option.map snd in
      compilation_unit_of_sig ~make_root ~imports ~interface ~name ~id sg
  | _ -> raise Corrupted

let read_impl ~make_root ~filename ~source_id () =
  match Cmt_format.read_cmt filename with
  | exception Cmi_format.Error (Not_an_interface _) ->
      raise Not_an_implementation
  | cmt_info -> (
      let name = cmt_info.cmt_modname |> Compilation_unit.name_as_string in
      let _sourcefile =
        ( cmt_info.cmt_sourcefile,
          cmt_info.cmt_source_digest,
          cmt_info.cmt_builddir )
      in
      let interface = cmt_info.cmt_interface_digest in
      let imports =
        cmt_info.cmt_imports
        |> Array.map (fun import ->
          Import_info.name import |> Compilation_unit.Name.to_string,
          Import_info.crc import)
        |> Array.to_list
      in
      match cmt_info.cmt_annots with
      | Implementation _impl ->
          let digest =
            match interface with
            | Some digest -> digest
            | None -> (
                match List.assoc name imports with
                | Some digest -> digest
                | None -> raise Corrupted
                | exception Not_found -> raise Corrupted)
          in
          let () =
            match source_id with
            | None -> Odoc_model.Names.set_unique_ident filename
            | Some source_id ->
                Odoc_model.Names.set_unique_ident
                  (Odoc_model.Paths.Identifier.fullname source_id
                  |> String.concat "-")
          in
          let root =
            match make_root ~module_name:name ~digest with
            | Ok root -> root
            | Error (`Msg m) -> raise (Make_root_error m)
          in
          let imports = List.filter (fun (name', _) -> name <> name') imports in
          let imports =
            List.map
              (fun (s, d) ->
                Odoc_model.Lang.Compilation_unit.Import.Unresolved (s, d))
              imports
          in
          read_cmt_infos source_id ~filename root digest imports ()
      | _ -> raise Not_an_implementation)

(** Catch errors from reading the object files and some internal errors *)
let wrap_errors ~filename f =
  Odoc_model.Error.catch_errors_and_warnings (fun () ->
      try f () with
      | Cmi_format.Error (Not_an_interface _) -> not_an_interface filename
      | Cmt_format.Error (Not_a_typedtree _) -> not_a_typedtree filename
      | Cmi_format.Error (Wrong_version_interface _) -> wrong_version filename
      | Cmi_format.Error (Corrupted_interface _) -> corrupted filename
      | Corrupted -> corrupted filename
      | Not_an_implementation -> not_an_implementation filename
      | Not_an_interface -> not_an_interface filename
      | Make_root_error m -> error_msg filename m)

let read_cmti ~make_root ~parent ~filename ~warnings_tag =
  wrap_errors ~filename (read_cmti ~make_root ~parent ~filename ~warnings_tag)

let read_cmt ~make_root ~parent ~filename ~warnings_tag =
  wrap_errors ~filename (read_cmt ~make_root ~parent ~filename ~warnings_tag)

let read_impl ~make_root ~filename ~source_id =
  wrap_errors ~filename (read_impl ~make_root ~source_id ~filename)

let read_cmi ~make_root ~parent ~filename ~warnings_tag =
  wrap_errors ~filename (read_cmi ~make_root ~parent ~filename ~warnings_tag)

let read_location = Doc_attr.read_location

let parse_attribute = Doc_attr.parse_attribute
