#if OCAML_VERSION >= (4, 14, 0)

module Analysis = struct
  type annotation =
    | LocalDefinition of Ident.t
    | Value of Path.t
    | Module of Path.t
    | ClassType of Path.t
    | ModuleType of Path.t
    | Type of Path.t
    | Constructor of Path.t

  let expr poses expr =
    let exp_loc = expr.Typedtree.exp_loc in
    if exp_loc.loc_ghost then ()
    else
      match expr.exp_desc with
      | Texp_ident (p, _, _) -> poses := (Value p, exp_loc) :: !poses
      | Texp_construct (_, { cstr_res; _ }, _) -> (
          let desc = Types.get_desc cstr_res in
          match desc with
          | Types.Tconstr (p, _, _) ->
              poses := (Constructor p, exp_loc) :: !poses
          | _ -> ())
      | _ -> ()

  let pat env (type a) poses : a Typedtree.general_pattern -> unit = function
    | { Typedtree.pat_desc; pat_loc; _ } when not pat_loc.loc_ghost ->
        let () =
          match pat_desc with
          | Typedtree.Tpat_construct (_, { cstr_res; _ }, _, _) -> (
              let desc = Types.get_desc cstr_res in
              match desc with
              | Types.Tconstr (p, _, _) ->
                  poses := (Constructor p, pat_loc) :: !poses
              | _ -> ())
          | _ -> ()
        in
        let maybe_localvalue id loc =
          match Ident_env.identifier_of_loc env loc with
          | None -> Some (LocalDefinition id, loc)
          | Some _ -> None
        in
        let () =
          match pat_desc with
          | Tpat_var (id, loc) -> (
              match maybe_localvalue id loc.loc with
              | Some x -> poses := x :: !poses
              | None -> ())
          | Tpat_alias (_, id, loc) -> (
              match maybe_localvalue id loc.loc with
              | Some x -> poses := x :: !poses
              | None -> ())
          | _ -> ()
        in
        ()
    | _ -> ()

  (* Add module_binding equivalent of pat *)


  let module_expr poses mod_expr =
    match mod_expr with
    | { Typedtree.mod_desc = Tmod_ident (p, _); mod_loc; _ }
      when not mod_loc.loc_ghost ->
        poses := (Module p, mod_loc) :: !poses
    | _ -> ()

  let class_type poses cltyp =
    match cltyp with
    | { Typedtree.cltyp_desc = Tcty_constr (p, _, _); cltyp_loc; _ }
      when not cltyp_loc.loc_ghost ->
        poses := (ClassType p, cltyp_loc) :: !poses
    | _ -> ()

  let module_type poses mty_expr =
    match mty_expr with
    | { Typedtree.mty_desc = Tmty_ident (p, _); mty_loc; _ }
      when not mty_loc.loc_ghost ->
        poses := (ModuleType p, mty_loc) :: !poses
    | _ -> ()

  let core_type poses ctyp_expr =
    match ctyp_expr with
    | { Typedtree.ctyp_desc = Ttyp_constr (p, _, _); ctyp_loc; _ }
      when not ctyp_loc.loc_ghost ->
        poses := (Type p, ctyp_loc) :: !poses
    | _ -> ()
end

let of_cmt env structure =
  let poses = ref [] in
  let module_expr iterator mod_expr =
    Analysis.module_expr poses mod_expr;
    Tast_iterator.default_iterator.module_expr iterator mod_expr
  in
  let expr iterator e =
    Analysis.expr poses e;
    Tast_iterator.default_iterator.expr iterator e
  in
  let pat iterator e =
    Analysis.pat env poses e;
    Tast_iterator.default_iterator.pat iterator e
  in
  let typ iterator ctyp_expr =
    Analysis.core_type poses ctyp_expr;
    Tast_iterator.default_iterator.typ iterator ctyp_expr
  in
  let module_type iterator mty =
    Analysis.module_type poses mty;
    Tast_iterator.default_iterator.module_type iterator mty
  in
  let class_type iterator cl_type =
    Analysis.class_type poses cl_type;
    Tast_iterator.default_iterator.class_type iterator cl_type
  in
  let iterator =
    {
      Tast_iterator.default_iterator with
      expr;
      pat;
      module_expr;
      typ;
      module_type;
      class_type;
    }
  in
  iterator.structure iterator structure;
  !poses

#else

#endif
