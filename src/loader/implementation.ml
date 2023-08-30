#if OCAML_VERSION >= (4, 14, 0)

(* open Odoc_model.Lang.Source_info *)

let pos_of_loc loc = (loc.Location.loc_start.pos_cnum, loc.loc_end.pos_cnum)

type annotations =
  | LocalDefinition of Ident.t
  | LocalValue of Ident.t
  | DefJmp of Shape.Uid.t

module Analysis = struct
  let ( @ ) = List.rev_append

  open Typedtree
  open Odoc_model.Paths

  type env = Ident_env.t * Warnings.loc Shape.Uid.Tbl.t

  let env_wrap : (Ident_env.t -> Ident_env.t) -> env -> env =
   fun f (env, uid_to_loc) -> (f env, uid_to_loc)

  let get_env : env -> Ident_env.t = fun (env, _) -> env

  let get_uid_to_loc : env -> Warnings.loc Shape.Uid.Tbl.t =
   fun (_, uid_to_loc) -> uid_to_loc

  let rec structure env parent str =
    let env' = env_wrap (Ident_env.add_structure_tree_items parent str) env in
    List.fold_left
      (fun items item ->
        List.rev_append (structure_item env' parent item) items)
      [] str.str_items
    |> List.rev

  and signature env parent sg =
    let env' = env_wrap (Ident_env.add_signature_tree_items parent sg) env in
    List.fold_left
      (fun items item ->
        List.rev_append (signature_item env' parent item) items)
      [] sg.sig_items
    |> List.rev

  and signature_item env parent item =
    match item.sig_desc with
    | Tsig_value vd -> value_description env parent vd
    | Tsig_type (_, tds) -> type_declarations env parent tds
    | Tsig_typesubst tds -> type_declarations env parent tds
    | Tsig_typext _ -> []
    | Tsig_exception e -> exception_ env parent e
    | Tsig_module mb -> module_declaration env parent mb
    | Tsig_modsubst ms -> module_substitution env parent ms
    | Tsig_recmodule mbs -> module_declarations env parent mbs
    | Tsig_modtype mtd -> module_type_declaration env parent mtd
    | Tsig_modtypesubst mtd -> module_type_declaration env parent mtd
    | Tsig_open _ -> []
    | Tsig_include _ -> []
    | Tsig_class cd -> class_description env parent cd
    | Tsig_class_type ctd -> class_type_declaration env parent ctd
    | Tsig_attribute _ -> []

  and value_description _env _parent _vd = []

  (* and type_declaration _env _parent _td = [] *)

  and type_declarations _env _parent _tds = []

  and exception_ _env _parent _e = []

  and module_declaration env _parent md =
    match md.md_id with
    | None -> []
    | Some mb_id ->
        let id = Ident_env.find_module_identifier (get_env env) mb_id in
        module_type env (id :> Identifier.Signature.t) md.md_type

  and module_declarations env parent mds =
    List.fold_left
      (fun items md -> List.rev_append (module_declaration env parent md) items)
      [] mds
    |> List.rev

  and module_substitution _env _parent _ms = []

  and module_type_declaration env _parent mtd =
    let id = Ident_env.find_module_type (get_env env) mtd.mtd_id in
    match mtd.mtd_type with
    | None -> []
    | Some mty -> module_type env (id :> Identifier.Signature.t) mty

  and class_description _env _parent _cd = []

  and class_type_declaration _env _parent _ctd = []

  and structure_item env parent item =
    match item.str_desc with
    | Tstr_eval (e, _) -> expression env e
    | Tstr_value (_, vbs) -> value_bindings env parent vbs
    | Tstr_module mb -> module_binding env parent mb
    | Tstr_recmodule mbs -> module_bindings env parent mbs
    | Tstr_modtype mtd -> module_type_decl env parent mtd
    | Tstr_open _ -> []
    | Tstr_class _ -> []
    | Tstr_class_type _ -> []
    | Tstr_include _ -> []
    | Tstr_attribute _ -> []
    | Tstr_primitive _ -> []
    | Tstr_type (_, tds) -> type_declarations env parent tds
    | Tstr_typext _ -> []
    | Tstr_exception _ -> []

  and value_bindings env _parent vbs =
    let items =
      List.fold_left
        (fun acc vb ->
          let vb = value_binding env vb in
          List.rev_append vb acc)
        [] vbs
    in
    List.rev items

  and pattern : type a. env -> a general_pattern -> _ =
   fun env p ->
    let maybe_localvalue id loc =
      match Ident_env.identifier_of_loc (get_env env) loc with
      | None -> Some (LocalDefinition id, pos_of_loc loc)
      | Some _ -> None
    in
    match p.pat_desc with
    | Tpat_any -> []
    | Tpat_var (id, loc) -> (
        match maybe_localvalue id loc.loc with Some x -> [ x ] | None -> [])
    | Tpat_alias (p, id, loc) -> (
        match maybe_localvalue id loc.loc with
        | Some x -> x :: pattern env p
        | None -> pattern env p)
    | Tpat_constant _ -> []
    | Tpat_tuple ps ->
        List.fold_left (fun acc p -> List.rev_append (pattern env p) acc) [] ps
    | Tpat_construct (_, _, ps, _) ->
        List.fold_left (fun acc p -> List.rev_append (pattern env p) acc) [] ps
    | Tpat_variant (_, None, _) -> []
    | Tpat_variant (_, Some p, _) -> pattern env p
    | Tpat_record (fields, _) ->
        List.fold_left
          (fun acc (_, _, p) -> List.rev_append (pattern env p) acc)
          [] fields
    | Tpat_array ps ->
        List.fold_left (fun acc p -> List.rev_append (pattern env p) acc) [] ps
    | Tpat_or (p1, p2, _) -> List.rev_append (pattern env p1) (pattern env p2)
    | Tpat_lazy p -> pattern env p
    | Tpat_exception p -> pattern env p
    | Tpat_value p -> pattern env (p :> pattern)

  and value_binding env vb = pattern env vb.vb_pat @ expression env vb.vb_expr

  and expression env = function
    | { exp_desc = Texp_ident (p, _, value_description); exp_loc; _ } -> (
        if exp_loc.loc_ghost then []
        else
          (* Only generate anchor if the uid is in the location table. We don't
             link to modules outside of the compilation unit. *)
          match
            Shape.Uid.Tbl.find_opt (get_uid_to_loc env)
              value_description.val_uid
          with
          | Some _ -> [ (DefJmp value_description.val_uid, pos_of_loc exp_loc) ]
          | None -> (
              match p with
              | Pident id -> [ (LocalValue id, pos_of_loc exp_loc) ]
              | _ -> []))
    | { exp_desc = Texp_constant _; _ } -> []
    | { exp_desc = Texp_let (_, vbs, e); _ } ->
        List.concat_map (value_binding env) vbs @ expression env e
    | { exp_desc = Texp_function f; _ } -> List.concat_map (case env) f.cases
    | { exp_desc = Texp_match (e, cases, _); _ } ->
        expression env e @ List.concat_map (case env) cases
    | { exp_desc = Texp_try (e, cases); _ } ->
        expression env e @ List.concat_map (case env) cases
    | { exp_desc = Texp_tuple es; _ } -> List.concat_map (expression env) es
    | { exp_desc = Texp_construct (_, cons_description, es); exp_loc; _ } ->
        let x =
          if exp_loc.loc_ghost then []
          else
            match
              Shape.Uid.Tbl.find_opt (get_uid_to_loc env)
                cons_description.cstr_uid
            with
            | Some _ ->
                [ (DefJmp cons_description.cstr_uid, pos_of_loc exp_loc) ]
            | None -> []
        in
        x @ List.concat_map (expression env) es
    | { exp_desc = Texp_variant (_, Some e); _ } -> expression env e
    | { exp_desc = Texp_variant (_, None); _ } -> []
    | { exp_desc = Texp_record { fields; extended_expression; _ }; _ } ->
        let e =
          match extended_expression with
          | None -> []
          | Some expr -> expression env expr
        in
        e @ List.concat_map (record_fields env) (Array.to_list fields)
    | { exp_desc = Texp_field (e, _, _); _ } -> expression env e
    | { exp_desc = Texp_setfield (e1, _, _, e2); _ } ->
        expression env e1 @ expression env e2
    | { exp_desc = Texp_array es; _ } -> List.concat_map (expression env) es
    | { exp_desc = Texp_ifthenelse (e1, e2, e3); _ } ->
        let e3 = match e3 with Some e -> expression env e | None -> [] in
        e3 @ expression env e1 @ expression env e2
    | { exp_desc = Texp_sequence (e1, e2); _ } ->
        expression env e1 @ expression env e2
    | { exp_desc = Texp_while (e1, e2); _ } ->
        expression env e1 @ expression env e2
    | { exp_desc = Texp_for (id, p, e1, e2, _, e3); _ } ->
        ((LocalValue id, pos_of_loc p.ppat_loc) :: expression env e1)
        @ expression env e2 @ expression env e3
    | { exp_desc = Texp_send (e, _); _ } -> expression env e
    | { exp_desc = Texp_new _; _ } -> []
    | { exp_desc = Texp_instvar (_, _, _); _ } -> []
    | { exp_desc = Texp_setinstvar (_, _, _, e); _ } -> expression env e
    | { exp_desc = Texp_override (_, es); _ } ->
        List.concat_map (fun (_, _, e) -> expression env e) es
    | { exp_desc = Texp_letmodule (_, _, _, _m, e); _ } -> expression env e
    | { exp_desc = Texp_letexception (_, e); _ } -> expression env e
    | { exp_desc = Texp_assert e; _ } -> expression env e
    | { exp_desc = Texp_lazy e; _ } -> expression env e
    | { exp_desc = Texp_object (_, _); _ } -> []
    | { exp_desc = Texp_pack _; _ } -> []
    | { exp_desc = Texp_letop { let_; ands; body; _ }; _ } ->
        let e = case env body in
        let let_ = binding_op env let_ in
        let ands = List.concat_map (binding_op env) ands in
        e @ let_ @ ands
    | { exp_desc = Texp_unreachable; _ } -> []
    | { exp_desc = Texp_extension_constructor _; _ } -> []
    | { exp_desc = Texp_open (_, e); _ } -> expression env e
    | { exp_desc = Texp_apply (e, es); _ } ->
        expression env e
        @ List.concat_map
            (function _, Some e -> expression env e | _ -> [])
            es

  and binding_op env = function { bop_exp; _ } -> expression env bop_exp

  and record_fields env f =
    match f with _, Overridden (_, e) -> expression env e | _, Kept _ -> []

  and case : type a. env -> a Typedtree.case -> _ =
   fun env c ->
    pattern env c.c_lhs
    @
    match c.c_guard with
    | None -> expression env c.c_rhs
    | Some e -> expression env e @ expression env c.c_rhs

  and module_type_decl env _parent mtd =
    let id = Ident_env.find_module_type (get_env env) mtd.mtd_id in
    match mtd.mtd_type with
    | None -> []
    | Some mty -> module_type env (id :> Identifier.Signature.t) mty

  and module_type env (parent : Identifier.Signature.t) mty =
    match mty.mty_desc with
    | Tmty_signature sg -> signature env (parent : Identifier.Signature.t) sg
    | Tmty_with (mty, _) -> module_type env parent mty
    | Tmty_ident _ -> []
    | Tmty_functor (_, t) -> module_type env parent t
    | Tmty_alias _ -> []
    | Tmty_typeof _ -> []

  and module_bindings env parent mbs =
    let items =
      List.fold_left
        (fun acc vb -> List.rev_append (module_binding env parent vb) acc)
        [] mbs
    in
    List.rev items

  and module_binding env _parent mb =
    match mb.mb_id with
    | None -> []
    | Some id ->
        let id = Ident_env.find_module_identifier (get_env env) id in
        let id = (id :> Identifier.Module.t) in
        let inner =
          match unwrap_module_expr_desc mb.mb_expr.mod_desc with
          | Tmod_ident (_p, _) -> []
          | _ ->
              let id = (id :> Identifier.Signature.t) in
              module_expr env id mb.mb_expr
        in
        inner

  and module_expr env parent mexpr =
    let open Odoc_model.Names in
    match mexpr.mod_desc with
    | Tmod_ident _ -> []
    | Tmod_structure str ->
        let sg = structure env parent str in
        sg
    | Tmod_functor (parameter, res) ->
        let _f_parameter, env =
          match parameter with
          | Unit -> ([], env)
          | Named (id_opt, _, _arg) ->
              let name, env =
                match id_opt with
                | Some id ->
                    ( Ident.name id,
                      env_wrap
                        (Ident_env.add_parameter parent id
                           (ModuleName.of_ident id))
                        env )
                | None -> ("_", env)
              in
              let _id =
                Odoc_model.Paths.Identifier.Mk.parameter
                  (parent, Odoc_model.Names.ModuleName.make_std name)
              in
              ([], env)
        in
        let res =
          module_expr env (Odoc_model.Paths.Identifier.Mk.result parent) res
        in
        res
    | Tmod_constraint (me, _, constr, _) ->
        let c =
          match constr with
          | Tmodtype_implicit -> []
          | Tmodtype_explicit mt -> module_type env parent mt
        in
        c @ module_expr env parent me
    | _ -> []

  and unwrap_module_expr_desc = function
    | Tmod_constraint (mexpr, _, Tmodtype_implicit, _) ->
        unwrap_module_expr_desc mexpr.mod_desc
    | desc -> desc
end

let postprocess_poses source_id poses uid_to_id uid_to_loc =
  let local_def_anchors =
    List.filter_map
      (function
        | LocalDefinition id, (start, _) ->
            let name =
              Odoc_model.Names.LocalName.make_std
                (Printf.sprintf "local_%s_%d" (Ident.name id) start)
            in
            let identifier =
              Odoc_model.Paths.Identifier.Mk.source_location_int
                (source_id, name)
            in
            Some (id, identifier)
        | _ -> None)
      poses
  in
  let poses =
    List.filter_map
      (function
        | LocalDefinition id, loc ->
            Some
              ( Odoc_model.Lang.Source_info.Definition
                  (List.assoc id local_def_anchors),
                loc )
        | LocalValue uniq, loc -> (
            match List.assoc_opt uniq local_def_anchors with
            | Some anchor -> Some (Value anchor, loc)
            | None -> None)
        | DefJmp x, loc -> (
            match Shape.Uid.Map.find_opt x uid_to_id with
            | Some id -> Some (Value id, loc)
            | None -> None))
      poses
  in
  let defs =
    Shape.Uid.Map.fold
      (fun uid id acc ->
        let loc_opt = Shape.Uid.Tbl.find_opt uid_to_loc uid in
        match loc_opt with
        | Some loc ->
            (Odoc_model.Lang.Source_info.Definition id, pos_of_loc loc) :: acc
        | _ -> acc)
      uid_to_id []
  in
  defs @ poses

let string_of_full_name_ty : Odoc_model.Paths.Identifier.full_name_ty -> string
    = function
  | `Page -> "page"
  | `Module -> "module"
  | `Constructor -> "constructor"
  | `Field -> "field"
  | `Extension -> "extension"
  | `Exception -> "exception"
  | `Value -> "value"
  | `Class -> "class"
  | `ClassType -> "class_type"
  | `Method -> "method"
  | `InstanceVariable -> "instance_variable"
  | `Label -> "label"
  | `ModuleType -> "module_type"
  | `Type -> "type"
  | `Parameter -> "parameter"
  | `Src -> "src"
  | `Asset -> "asset"

let anchor_of_identifier id =
  let full_name = Odoc_model.Paths.Identifier.full_name id in
  List.filter_map
    (fun (x, y) ->
      match x with
      | `Page -> None
      | `Src -> None
      | `Asset -> None
      | _ -> Some (Printf.sprintf "%s-%s" (string_of_full_name_ty x) y))
    full_name
  |> List.tl |> String.concat "."

let of_cmt (source_id_opt : Odoc_model.Paths.Identifier.SourcePage.t option)
    (id : Odoc_model.Paths.Identifier.RootModule.t) (cmt : Cmt_format.cmt_infos)
    =
  let ttree = cmt.cmt_annots in
  match (source_id_opt, ttree, cmt.cmt_impl_shape) with
  | Some source_id, Cmt_format.Implementation structure, Some shape ->
      let uid_to_loc = cmt.cmt_uid_to_loc in
      let env = Ident_env.empty () in
      let vs =
        Analysis.structure (env, uid_to_loc)
          (id :> Odoc_model.Paths.Identifier.Signature.t)
          structure
      in
      let uid_to_loc_map = Shape.Uid.Tbl.to_map uid_to_loc in
      let uid_to_id :
          Odoc_model.Paths.Identifier.SourceLocation.t Shape.Uid.Map.t =
        Shape.Uid.Map.filter_map
          (fun uid loc ->
            if loc.Location.loc_ghost then None
            else
              let identifier = Ident_env.identifier_of_loc env loc in
              let anchor =
                match identifier with
                | Some x ->
                    Some
                      (Odoc_model.Names.DefName.make_std
                         (anchor_of_identifier x))
                | None -> (
                    match uid with
                    | Compilation_unit _ -> None
                    | Item _ ->
                        let name =
                          Odoc_model.Names.DefName.make_std
                            (Printf.sprintf "def_%d_%d" loc.loc_start.pos_cnum
                               loc.loc_end.pos_cnum)
                        in
                        Some name
                    | _ -> None)
              in
              match anchor with
              | Some a ->
                  Some
                    (Odoc_model.Paths.Identifier.Mk.source_location
                       (source_id, a)
                      :> Odoc_model.Paths.Identifier.SourceLocation.t)
              | None -> None)
          uid_to_loc_map
      in

      ( Some (shape, uid_to_id),
        postprocess_poses source_id vs uid_to_id uid_to_loc )
  | None, _, Some shape ->
      (Some (shape, Shape.Uid.Map.empty), [] (* At least preserve the shape *))
  | _ -> (None, [])

#else

let of_cmt _ _ _ = None, []

#endif
