open Result
module Error = Odoc_model.Error

module Lookup_def = Lookup_def
module Source_info = Source_info

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

(** [cmt_info.cmt_annots = Implementation _] *)
let read_cmt_infos' cmt_info =
  match Lookup_def.of_cmt cmt_info with
  | None -> None
  | Some shape ->
      let jmp_infos = Local_jmp.of_cmt cmt_info in
      Some (shape, jmp_infos)

let read_cmt_infos ~filename () =
  match Cmt_format.read_cmt filename with
  | exception Cmi_format.Error _ -> raise Corrupted
  | cmt_info -> (
      match cmt_info.cmt_annots with
      | Implementation _ -> read_cmt_infos' cmt_info
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
    hidden = Odoc_model.Root.contains_double_underscore name;
    content;
    expansion = None;
    linked = false;
    canonical;
    source_info = None;
  }

let compilation_unit_of_sig ~make_root ~imports ~interface ?sourcefile ~name ~id
    ?canonical sg =
  let content = Odoc_model.Lang.Compilation_unit.Module sg in
  make_compilation_unit ~make_root ~imports ~interface ?sourcefile ~name ~id
    ?canonical content

let read_cmti ~make_root ~parent ~filename () =
  let cmt_info = Cmt_format.read_cmt filename in
  match cmt_info.cmt_annots with
  | Interface intf -> (
      match cmt_info.cmt_interface_digest with
      | None -> raise Corrupted
      | Some _ as interface ->
          let name = cmt_info.cmt_modname in
          let sourcefile =
            ( cmt_info.cmt_sourcefile,
              cmt_info.cmt_source_digest,
              cmt_info.cmt_builddir )
          in
          let id, sg, canonical = Cmti.read_interface parent name intf in
          compilation_unit_of_sig ~make_root ~imports:cmt_info.cmt_imports
            ~interface ~sourcefile ~name ~id ?canonical sg)
  | _ -> raise Not_an_interface

let read_cmt ~make_root ~parent ~filename () =
  match Cmt_format.read_cmt filename with
  | exception Cmi_format.Error (Not_an_interface _) ->
      raise Not_an_implementation
  | cmt_info -> (
      let name = cmt_info.cmt_modname in
      let sourcefile =
        ( cmt_info.cmt_sourcefile,
          cmt_info.cmt_source_digest,
          cmt_info.cmt_builddir )
      in
      let interface = cmt_info.cmt_interface_digest in
      let imports = cmt_info.cmt_imports in
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
                let path = `Root name in
                { Odoc_model.Lang.Compilation_unit.Packed.id; path })
              items
          in
          let content = Odoc_model.Lang.Compilation_unit.Pack items in
          ( make_compilation_unit ~make_root ~imports ~interface ~sourcefile
              ~name ~id content,
            None )
      | Implementation impl ->
          let id, sg, canonical = Cmt.read_implementation parent name impl in
          ( compilation_unit_of_sig ~make_root ~imports ~interface ~sourcefile
              ~name ~id ?canonical sg,
            read_cmt_infos' cmt_info )
      | _ -> raise Not_an_implementation)

let read_cmi ~make_root ~parent ~filename () =
  let cmi_info = Cmi_format.read_cmi filename in
  match cmi_info.cmi_crcs with
  | (name, (Some _ as interface)) :: imports when name = cmi_info.cmi_name ->
      let id, sg =
        Cmi.read_interface parent name
          (Odoc_model.Compat.signature cmi_info.cmi_sign)
      in
      compilation_unit_of_sig ~make_root ~imports ~interface ~name ~id sg
  | _ -> raise Corrupted

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

let read_cmt_infos ~filename = wrap_errors ~filename (read_cmt_infos ~filename)

let read_cmti ~make_root ~parent ~filename =
  wrap_errors ~filename (read_cmti ~make_root ~parent ~filename)

let read_cmt ~make_root ~parent ~filename =
  wrap_errors ~filename (read_cmt ~make_root ~parent ~filename)

let read_cmi ~make_root ~parent ~filename =
  wrap_errors ~filename (read_cmi ~make_root ~parent ~filename)

let read_location = Doc_attr.read_location
