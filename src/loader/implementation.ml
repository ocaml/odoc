#if OCAML_VERSION >= (4, 14, 0)

(* open Odoc_model.Lang.Source_info *)

let pos_of_loc loc = (loc.Location.loc_start.pos_cnum, loc.loc_end.pos_cnum)

type annotations =
  | LocalDefinition of Ident.t
  | LocalValue of Ident.t
  | DefJmp of Shape.Uid.t

module Analysis = struct
  open Typedtree
  open Odoc_model.Paths

  type env = Ident_env.t * Location.t Shape.Uid.Tbl.t

  let env_wrap : (Ident_env.t -> Ident_env.t) -> env -> env =
   fun f (env, uid_to_loc) -> (f env, uid_to_loc)

  let get_env : env -> Ident_env.t = fun (env, _) -> env

  let get_uid_to_loc : env -> Location.t Shape.Uid.Tbl.t =
   fun (_, uid_to_loc) -> uid_to_loc

  let rec structure env parent acc str =
    let env' = env_wrap (Ident_env.add_structure_tree_items parent str) env in
    List.fold_left (structure_item env' parent) acc str.str_items

  and signature env parent acc sg =
    let env' = env_wrap (Ident_env.add_signature_tree_items parent sg) env in
    List.fold_left (signature_item env' parent) acc sg.sig_items

  and signature_item env parent acc item =
    match item.sig_desc with
    | Tsig_value vd -> value_description env parent acc vd
    | Tsig_type (_, tds) -> type_declarations env parent acc tds
    | Tsig_typesubst tds -> type_declarations env parent acc tds
    | Tsig_typext _ -> acc
    | Tsig_exception e -> exception_ env parent acc e
    | Tsig_module mb -> module_declaration env parent acc mb
    | Tsig_modsubst ms -> module_substitution env parent acc ms
    | Tsig_recmodule mbs -> module_declarations env parent acc mbs
    | Tsig_modtype mtd -> module_type_declaration env parent acc mtd
    | Tsig_modtypesubst mtd -> module_type_declaration env parent acc mtd
    | Tsig_open _ -> acc
    | Tsig_include _ -> acc
    | Tsig_class cd -> class_description env parent acc cd
    | Tsig_class_type ctd -> class_type_declaration env parent acc ctd
    | Tsig_attribute _ -> acc

  and value_description _env _parent acc _vd = acc

  (* and type_declaration _env _parent _td = [] *)

  and type_declarations _env _parent acc _tds = acc

  and exception_ _env _parent acc _e = acc

  and module_declaration env _parent acc md =
    match md.md_id with
    | None -> acc
    | Some mb_id ->
        let id = Ident_env.find_module_identifier (get_env env) mb_id in
        module_type env (id :> Identifier.Signature.t) acc md.md_type

  and module_declarations env parent acc mds =
    List.fold_left (module_declaration env parent) acc mds

  and module_substitution _env _parent acc _ms = acc

  and module_type_declaration env _parent acc mtd =
    let id = Ident_env.find_module_type (get_env env) mtd.mtd_id in
    match mtd.mtd_type with
    | None -> acc
    | Some mty -> module_type env (id :> Identifier.Signature.t) acc mty

  and class_description _env _parent acc _cd = acc

  and class_type_declaration _env _parent acc _ctd = acc

  and structure_item env parent acc item =
    match item.str_desc with
    | Tstr_eval (e, _) -> expression env acc e
    | Tstr_value (_, vbs) -> value_bindings env parent acc vbs
    | Tstr_module mb -> module_binding env parent acc mb
    | Tstr_recmodule mbs -> module_bindings env parent acc mbs
    | Tstr_modtype mtd -> module_type_decl env parent acc mtd
    | Tstr_open _ -> acc
    | Tstr_class _ -> acc
    | Tstr_class_type _ -> acc
    | Tstr_include _ -> acc
    | Tstr_attribute _ -> acc
    | Tstr_primitive _ -> acc
    | Tstr_type (_, tds) -> type_declarations env parent acc tds
    | Tstr_typext _ -> acc
    | Tstr_exception _ -> acc

  and value_bindings env _parent acc vbs =
    List.fold_left (value_binding env) acc vbs

  and pattern : type a. env -> _ -> a general_pattern -> _ =
   fun env acc p ->
    let maybe_localvalue id loc =
      match Ident_env.identifier_of_loc (get_env env) loc with
      | None -> Some (LocalDefinition id, pos_of_loc loc)
      | Some _ -> None
    in
    match p.pat_desc with
    | Tpat_any -> acc
    | Tpat_var (id, loc) -> (
        match maybe_localvalue id loc.loc with
        | Some x -> x :: acc
        | None -> acc)
    | Tpat_alias (p, id, loc) -> (
        match maybe_localvalue id loc.loc with
        | Some x -> x :: pattern env acc p
        | None -> pattern env acc p)
    | Tpat_constant _ -> acc
    | Tpat_tuple ps -> List.fold_left (pattern env) acc ps
    | Tpat_construct (_, _, ps, _) -> List.fold_left (pattern env) acc ps
    | Tpat_variant (_, None, _) -> acc
    | Tpat_variant (_, Some p, _) -> pattern env acc p
    | Tpat_record (fields, _) ->
        List.fold_left (fun acc (_, _, p) -> pattern env acc p) acc fields
    | Tpat_array ps -> List.fold_left (pattern env) acc ps
    | Tpat_or (p1, p2, _) ->
        let acc = pattern env acc p1 in
        pattern env acc p2
    | Tpat_lazy p -> pattern env acc p
    | Tpat_exception p -> pattern env acc p
    | Tpat_value p -> pattern env acc (p :> pattern)

  and value_binding env acc vb =
    let acc = pattern env acc vb.vb_pat in
    expression env acc vb.vb_expr

  and expression env acc { exp_desc; exp_loc; _ } =
    match exp_desc with
    | Texp_ident (p, _, value_description) -> (
        if exp_loc.loc_ghost then acc
        else
          (* Only generate anchor if the uid is in the location table. We don't
             link to modules outside of the compilation unit. *)
          match
            Shape.Uid.Tbl.find_opt (get_uid_to_loc env)
              value_description.val_uid
          with
          | Some _ ->
              (DefJmp value_description.val_uid, pos_of_loc exp_loc) :: acc
          | None -> (
              match p with
              | Pident id -> (LocalValue id, pos_of_loc exp_loc) :: acc
              | _ -> acc))
    | Texp_constant _ -> acc
    | Texp_let (_, vbs, e) ->
        let acc = List.fold_left (value_binding env) acc vbs in
        expression env acc e
    | Texp_function f -> List.fold_left (case env) acc f.cases
    | Texp_match (e, cases, _) ->
        let acc = expression env acc e in
        List.fold_left (case env) acc cases
    | Texp_try (e, cases) ->
        let acc = expression env acc e in
        List.fold_left (case env) acc cases
    | Texp_tuple es -> List.fold_left (expression env) acc es
    | Texp_construct (_, cons_description, es) ->
        let acc =
          if exp_loc.loc_ghost then acc
          else
            match
              Shape.Uid.Tbl.find_opt (get_uid_to_loc env)
                cons_description.cstr_uid
            with
            | Some _ ->
                (DefJmp cons_description.cstr_uid, pos_of_loc exp_loc) :: acc
            | None -> acc
        in
        List.fold_left (expression env) acc es
    | Texp_variant (_, Some e) -> expression env acc e
    | Texp_variant (_, None) -> acc
    | Texp_record { fields; extended_expression; _ } ->
        let acc =
          match extended_expression with
          | None -> acc
          | Some expr -> expression env acc expr
        in
        List.fold_left (record_fields env) acc (Array.to_list fields)
    | Texp_field (e, _, _) -> expression env acc e
    | Texp_setfield (e1, _, _, e2) ->
        let acc = expression env acc e1 in
        expression env acc e2
    | Texp_array es -> List.fold_left (expression env) acc es
    | Texp_ifthenelse (e1, e2, e3) ->
        let acc =
          match e3 with Some e -> expression env acc e | None -> acc
        in
        let acc = expression env acc e1 in
        expression env acc e2
    | Texp_sequence (e1, e2) ->
        let acc = expression env acc e1 in
        expression env acc e2
    | Texp_while (e1, e2) ->
        let acc = expression env acc e1 in
        expression env acc e2
    | Texp_for (id, p, e1, e2, _, e3) ->
        let acc = (LocalValue id, pos_of_loc p.ppat_loc) :: acc in
        let acc = expression env acc e1 in
        let acc = expression env acc e2 in
        expression env acc e3
    | Texp_send (e, _) -> expression env acc e
    | Texp_new _ -> acc
    | Texp_instvar (_, _, _) -> acc
    | Texp_setinstvar (_, _, _, e) -> expression env acc e
    | Texp_override (_, es) ->
        List.fold_left (fun acc (_, _, e) -> expression env acc e) acc es
    | Texp_letmodule (_, _, _, _m, e) -> expression env acc e
    | Texp_letexception (_, e) -> expression env acc e
#if  OCAML_VERSION < (5,1,0)
    | Texp_assert e
#else
    | Texp_assert (e, _)
#endif
         -> expression env acc e
    | Texp_lazy e -> expression env acc e
    | Texp_object (_, _) -> acc
    | Texp_pack _ -> acc
    | Texp_letop { let_; ands; body; _ } ->
        let acc = case env acc body in
        let acc = binding_op env acc let_ in
        List.fold_left (binding_op env) acc ands
    | Texp_unreachable -> acc
    | Texp_extension_constructor _ -> acc
    | Texp_open (_, e) -> expression env acc e
    | Texp_apply (e, es) ->
        let acc = expression env acc e in
        List.fold_left
          (fun acc -> function _, Some e -> expression env acc e | _ -> acc)
          acc es

  and binding_op env acc = function
    | { bop_exp; _ } -> expression env acc bop_exp

  and record_fields env acc f =
    match f with
    | _, Overridden (_, e) -> expression env acc e
    | _, Kept _ -> acc

  and case : type a. env -> _ -> a Typedtree.case -> _ =
   fun env acc c ->
    let acc = pattern env acc c.c_lhs in
    match c.c_guard with
    | None -> expression env acc c.c_rhs
    | Some e ->
        let acc = expression env acc e in
        expression env acc c.c_rhs

  and module_type_decl env _parent acc mtd =
    let id = Ident_env.find_module_type (get_env env) mtd.mtd_id in
    match mtd.mtd_type with
    | None -> acc
    | Some mty -> module_type env (id :> Identifier.Signature.t) acc mty

  and module_type env (parent : Identifier.Signature.t) acc mty =
    match mty.mty_desc with
    | Tmty_signature sg ->
        signature env (parent : Identifier.Signature.t) acc sg
    | Tmty_with (mty, _) -> module_type env parent acc mty
    | Tmty_ident _ -> acc
    | Tmty_functor (_, t) -> module_type env parent acc t
    | Tmty_alias _ -> acc
    | Tmty_typeof _ -> acc

  and module_bindings env parent acc mbs =
    List.fold_left (module_binding env parent) acc mbs

  and module_binding env _parent acc mb =
    match mb.mb_id with
    | None -> acc
    | Some id ->
        let id = Ident_env.find_module_identifier (get_env env) id in
        let id = (id :> Identifier.Module.t) in
        let inner =
          match unwrap_module_expr_desc mb.mb_expr.mod_desc with
          | Tmod_ident (_p, _) -> acc
          | _ ->
              let id = (id :> Identifier.Signature.t) in
              module_expr env id acc mb.mb_expr
        in
        inner

  and module_expr env parent acc mexpr =
    match mexpr.mod_desc with
    | Tmod_ident _ -> acc
    | Tmod_structure str -> structure env parent acc str
    | Tmod_functor (parameter, res) ->
        let open Odoc_model.Names in
        let acc, env =
          match parameter with
          | Unit -> (acc, env)
          | Named (id_opt, _, arg) -> (
              match id_opt with
              | Some id ->
                  let env =
                    env_wrap
                      (Ident_env.add_parameter parent id
                         (ModuleName.of_ident id))
                      env
                  in
                  let id = Ident_env.find_module_identifier (get_env env) id in
                  (module_type env (id :> Identifier.Signature.t) acc arg, env)
              | None -> (acc, env))
        in
        module_expr env (Odoc_model.Paths.Identifier.Mk.result parent) acc res
    | Tmod_constraint (me, _, constr, _) ->
        let acc =
          match constr with
          | Tmodtype_implicit -> acc
          | Tmodtype_explicit mt -> module_type env parent acc mt
        in
        module_expr env parent acc me
    | _ -> acc

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
  in
  anchor_of_identifier [] id |> String.concat "."

let of_cmt (source_id : Odoc_model.Paths.Identifier.SourcePage.t)
    (id : Odoc_model.Paths.Identifier.RootModule.t)
    (structure : Typedtree.structure)
    (uid_to_loc : Warnings.loc Types.Uid.Tbl.t) =
  let env = Ident_env.empty () in
  let vs =
    Analysis.structure (env, uid_to_loc)
      (id :> Odoc_model.Paths.Identifier.Signature.t)
      [] structure
  in
  let uid_to_loc_map = Shape.Uid.Tbl.to_map uid_to_loc in
  let uid_to_id : Odoc_model.Paths.Identifier.SourceLocation.t Shape.Uid.Map.t =
    Shape.Uid.Map.filter_map
      (fun uid loc ->
        if loc.Location.loc_ghost then None
        else
          let identifier = Ident_env.identifier_of_loc env loc in
          let anchor =
            match identifier with
            | Some x ->
                Some
                  (Odoc_model.Names.DefName.make_std (anchor_of_identifier x))
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
                (Odoc_model.Paths.Identifier.Mk.source_location (source_id, a)
                  :> Odoc_model.Paths.Identifier.SourceLocation.t)
          | None -> None)
      uid_to_loc_map
  in

  (uid_to_id, postprocess_poses source_id vs uid_to_id uid_to_loc)

#else

let of_cmt _ _ _ _ = (), []

#endif
