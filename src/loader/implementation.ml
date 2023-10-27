#if OCAML_VERSION >= (4, 14, 0)

(* open Odoc_model.Lang.Source_info *)

let pos_of_loc loc = (loc.Location.loc_start.pos_cnum, loc.loc_end.pos_cnum)

let counter =
  let c = ref 0 in
  fun () ->
    incr c;
    !c

module Env = struct
  open Typedtree
  open Odoc_model.Paths

  let rec structure env parent str =
    let env' = Ident_env.add_structure_tree_items parent str env in
    List.iter (structure_item env' parent) str.str_items

  and signature env parent sg =
    let env' = Ident_env.add_signature_tree_items parent sg env in
    List.iter (signature_item env' parent) sg.sig_items

  and signature_item env parent item =
    match item.sig_desc with
    | Tsig_module mb -> module_declaration env parent mb
    | Tsig_recmodule mbs -> module_declarations env parent mbs
    | Tsig_modtype mtd -> module_type_declaration env parent mtd
    | Tsig_modtypesubst mtd -> module_type_declaration env parent mtd
    | Tsig_value _ | Tsig_type _ | Tsig_typesubst _ | Tsig_typext _
    | Tsig_exception _ | Tsig_modsubst _ | Tsig_open _ | Tsig_include _
    | Tsig_class _ | Tsig_class_type _ | Tsig_attribute _ ->
        ()

  and module_declaration env _parent md =
    match md.md_id with
    | None -> ()
    | Some mb_id ->
        let id = Ident_env.find_module_identifier env mb_id in
        module_type env (id :> Identifier.Signature.t) md.md_type

  and module_declarations env parent mds =
    List.iter (module_declaration env parent) mds

  and module_type_declaration env _parent mtd =
    let id = Ident_env.find_module_type env mtd.mtd_id in
    match mtd.mtd_type with
    | None -> ()
    | Some mty -> module_type env (id :> Identifier.Signature.t) mty

  and structure_item env parent item =
    match item.str_desc with
    | Tstr_module mb -> module_binding env parent mb
    | Tstr_recmodule mbs -> module_bindings env parent mbs
    | Tstr_modtype mtd -> module_type_decl env parent mtd
    | Tstr_open _ | Tstr_value _ | Tstr_class _ | Tstr_eval _
    | Tstr_class_type _ | Tstr_include _ | Tstr_attribute _ | Tstr_primitive _
    | Tstr_type _ | Tstr_typext _ | Tstr_exception _ ->
        ()

  and module_type_decl env _parent mtd =
    let id = Ident_env.find_module_type env mtd.mtd_id in
    match mtd.mtd_type with
    | None -> ()
    | Some mty -> module_type env (id :> Identifier.Signature.t) mty

  and module_type env (parent : Identifier.Signature.t) mty =
    match mty.mty_desc with
    | Tmty_signature sg -> signature env (parent : Identifier.Signature.t) sg
    | Tmty_with (mty, _) -> module_type env parent mty
    | Tmty_functor (_, t) -> module_type env parent t
    | Tmty_ident _ | Tmty_alias _ | Tmty_typeof _ -> ()

  and module_bindings env parent mbs = List.iter (module_binding env parent) mbs

  and module_binding env _parent mb =
    match mb.mb_id with
    | None -> ()
    | Some id ->
        let id = Ident_env.find_module_identifier env id in
        let id = (id :> Identifier.Module.t) in
        let inner =
          match unwrap_module_expr_desc mb.mb_expr.mod_desc with
          | Tmod_ident (_p, _) -> ()
          | _ ->
              let id = (id :> Identifier.Signature.t) in
              module_expr env id mb.mb_expr
        in
        inner

  and module_expr env parent mexpr =
    match mexpr.mod_desc with
    | Tmod_ident _ -> ()
    | Tmod_structure str -> structure env parent str
    | Tmod_functor (parameter, res) ->
        let open Odoc_model.Names in
        let env =
          match parameter with
          | Unit -> env
          | Named (id_opt, _, arg) -> (
              match id_opt with
              | Some id ->
                  let env =
                    Ident_env.add_parameter parent id (ModuleName.of_ident id)
                      env
                  in
                  let id = Ident_env.find_module_identifier env id in
                  module_type env (id :> Identifier.Signature.t) arg;
                  env
              | None -> env)
        in
        module_expr env (Odoc_model.Paths.Identifier.Mk.result parent) res
    | Tmod_constraint (me, _, constr, _) ->
        let () =
          match constr with
          | Tmodtype_implicit -> ()
          | Tmodtype_explicit mt -> module_type env parent mt
        in
        module_expr env parent me
    | _ -> ()

  and unwrap_module_expr_desc = function
    | Tmod_constraint (mexpr, _, Tmodtype_implicit, _) ->
        unwrap_module_expr_desc mexpr.mod_desc
    | desc -> desc

  let of_structure (id : Odoc_model.Paths.Identifier.RootModule.t)
      (s : Typedtree.structure) =
    let env = Ident_env.empty () in
    let () = structure env (id :> Odoc_model.Paths.Identifier.Signature.t) s in
    env
end

module LocHashtbl = Hashtbl.Make (struct
  type t = Location.t
  let equal l1 l2 = l1 = l2
  let hash = Hashtbl.hash
end)

module IdentHashtbl = Hashtbl.Make (struct
  type t = Ident.t
  let equal l1 l2 = l1 = l2
  let hash = Hashtbl.hash
end)

(* generate a [loc_to_id] and an [ident_to_id] map *)
let local_maps source_id poses =
  let loc_tbl = LocHashtbl.create 10 and ident_to_id = IdentHashtbl.create 10 in
  List.iter
    (function
      | Typedtree_traverse.Analysis.Definition id, loc ->
          let name =
            Odoc_model.Names.LocalName.make_std
              (Printf.sprintf "local_%s_%d" (Ident.name id) (counter ()))
          in
          let identifier =
            Odoc_model.Paths.Identifier.Mk.source_location_int (source_id, name)
          in
          IdentHashtbl.add ident_to_id id identifier;
          LocHashtbl.add loc_tbl loc identifier
      | _ -> ())
    poses;
  (loc_tbl, ident_to_id)

let postprocess_poses poses ident_to_id loc_to_id uid_to_id uid_to_loc :
    Odoc_model.Lang.Source_info.infos =
  let poses =
    List.filter_map
      (function
        | Typedtree_traverse.Analysis.Definition id, loc ->
            Some
              ( Odoc_model.Lang.Source_info.Definition
                  (IdentHashtbl.find ident_to_id id),
                pos_of_loc loc )
        | Value (LocalValue uniq), loc -> (
            match IdentHashtbl.find_opt ident_to_id uniq with
            | Some anchor -> Some (Value anchor, pos_of_loc loc)
            | None -> None)
        | Value (DefJmp x), loc -> (
            match Shape.Uid.Map.find_opt x uid_to_id with
            | Some id -> Some (Value id, pos_of_loc loc)
            | None -> None))
      poses
  in
  let defs =
    Shape.Uid.Map.fold
      (fun uid id acc ->
        let loc_opt = Shape.Uid.Tbl.find_opt uid_to_loc uid in
        match loc_opt with
        | Some loc when not (LocHashtbl.mem loc_to_id loc) ->
            (Odoc_model.Lang.Source_info.Definition id, pos_of_loc loc) :: acc
        | _ -> acc)
      uid_to_id []
  in
  defs @ poses

let anchor_of_identifier id =
  let open Odoc_document.Url in
  let open Odoc_model.Paths in
  let open Odoc_model.Names in
  let rec anchor_of_identifier acc (id : Identifier.t) =
    let continue anchor parent =
      anchor_of_identifier (anchor :: acc) (parent :> Identifier.t)
    in
    let anchor kind name =
      Printf.sprintf "%s-%s" (Anchor.string_of_kind kind) name
    in
    match id.iv with
    | `InstanceVariable (parent, name) ->
        let anchor = anchor `Val (InstanceVariableName.to_string name) in
        continue anchor parent
    | `Parameter (parent, name) as iv ->
        let arg_num =
          Identifier.FunctorParameter.functor_arg_pos { id with iv }
        in
        let kind = `Parameter arg_num in
        let anchor = anchor kind (ModuleName.to_string name) in
        continue anchor parent
    | `Module (parent, name) ->
        let anchor = anchor `Module (ModuleName.to_string name) in
        continue anchor parent
    | `SourceDir _ -> assert false
    | `ModuleType (parent, name) ->
        let anchor = anchor `ModuleType (ModuleTypeName.to_string name) in
        continue anchor parent
    | `Method (parent, name) ->
        let anchor = anchor `Method (MethodName.to_string name) in
        continue anchor parent
    | `AssetFile _ -> assert false
    | `Field (parent, name) ->
        let anchor = anchor `Field (FieldName.to_string name) in
        continue anchor parent
    | `SourceLocationMod _ -> assert false
    | `Result parent -> anchor_of_identifier acc (parent :> Identifier.t)
    | `SourceLocationInternal _ -> assert false
    | `Type (parent, name) ->
        let anchor = anchor `Type (TypeName.to_string name) in
        continue anchor parent
    | `Label _ -> assert false
    | `Exception (parent, name) ->
        let anchor = anchor `Exception (ExceptionName.to_string name) in
        continue anchor parent
    | `Class (parent, name) ->
        let anchor = anchor `Class (ClassName.to_string name) in
        continue anchor parent
    | `Page _ -> assert false
    | `LeafPage _ -> assert false
    | `CoreType _ -> assert false
    | `SourceLocation _ -> assert false
    | `ClassType (parent, name) ->
        let anchor = anchor `ClassType (ClassTypeName.to_string name) in
        continue anchor parent
    | `SourcePage _ -> assert false
    | `Value (parent, name) ->
        let anchor = anchor `Val (ValueName.to_string name) in
        continue anchor parent
    | `CoreException _ -> assert false
    | `Constructor (parent, name) ->
        let anchor = anchor `Constructor (ConstructorName.to_string name) in
        continue anchor parent
    | `Root _ ->
        (* We do not need to include the "container" root module in the anchor
           to have unique anchors. *)
        acc
    | `Extension (parent, name) ->
        let anchor = anchor `Extension (ExtensionName.to_string name) in
        continue anchor parent
    | `ExtensionDecl (parent, name, _) ->
        let anchor = anchor `ExtensionDecl (ExtensionName.to_string name) in
        continue anchor parent
  in
  anchor_of_identifier [] id |> String.concat "."

(** Returns an environment (containing a [loc] to [id] map) and an [uid] to [id]
    maps. *)
let id_map_of_cmt env (source_id : Odoc_model.Paths.Identifier.SourcePage.t)
    (uid_to_loc : Warnings.loc Types.Uid.Tbl.t) local_loc_to_id =
  let uid_to_loc_map = Shape.Uid.Tbl.to_map uid_to_loc in
  let mk_id name =
    Some
      (Odoc_model.Paths.Identifier.Mk.source_location (source_id, name)
        :> Odoc_model.Paths.Identifier.SourceLocation.t)
  in
  let uid_to_id : Odoc_model.Paths.Identifier.SourceLocation.t Shape.Uid.Map.t =
    Shape.Uid.Map.filter_map
      (fun uid loc ->
        if loc.Location.loc_ghost then None
        else
          let identifier = Ident_env.identifier_of_loc env loc in
          match identifier with
          | Some x ->
              mk_id (Odoc_model.Names.DefName.make_std (anchor_of_identifier x))
          | None -> (
              match uid with
              | Compilation_unit _ -> None
              | Item _ -> (
                  match LocHashtbl.find_opt local_loc_to_id loc with
                  | Some anchor -> Some anchor
                  | None ->
                      let name =
                        Odoc_model.Names.DefName.make_std
                          (Printf.sprintf "def_%d" (counter ()))
                      in
                      mk_id name)
              | _ -> None))
      uid_to_loc_map
  in
  uid_to_id

let read_cmt_infos source_id_opt id cmt_info =
  match Odoc_model.Compat.shape_of_cmt_infos cmt_info with
  | Some shape -> (
      let uid_to_loc = cmt_info.cmt_uid_to_loc in
      match (source_id_opt, cmt_info.cmt_annots) with
      | Some source_id, Implementation impl ->
          let env = Env.of_structure id impl in
          let occ_infos =
            Typedtree_traverse.of_cmt env uid_to_loc impl |> List.rev
            (* Information are accumulated in a list. We need to have the
               first info first in the list, to assign anchors with increasing
               numbers, so that adding some content at the end of a file does
               not modify the anchors for existing anchors. *)
          in
          let loc_to_id, ident_to_id = local_maps source_id occ_infos in
          let map = id_map_of_cmt env source_id uid_to_loc loc_to_id in
          let source_infos =
            postprocess_poses occ_infos ident_to_id loc_to_id map uid_to_loc
          in
          ( Some (shape, map),
            Some
              {
                Odoc_model.Lang.Source_info.id = source_id;
                infos = source_infos;
              } )
      | _, _ -> (Some (shape, Odoc_model.Compat.empty_map), None))
  | None -> (None, None)

#else

let read_cmt_infos _source_id_opt _id _cmt_info =
  (None, None)

#endif
