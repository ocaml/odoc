#if OCAML_VERSION >= (4, 14, 0)

module Analysis = struct
  type annotation =
    | LocalDefinition of Ident.t
    | Value of Path.t
    | Module of Path.t
    | ModuleType of Path.t
    | Type of Path.t

  let expr poses expr =
    let exp_loc = expr.Typedtree.exp_loc in
    if exp_loc.loc_ghost then ()
    else
      match expr.exp_desc with
      | Texp_ident (p, _, _, _, _) -> poses := (Value p, exp_loc) :: !poses
      | _ -> ()

  let pat env (type a) poses : a Typedtree.general_pattern -> unit = function
    | { Typedtree.pat_desc; pat_loc; _ } when not pat_loc.loc_ghost ->
        let maybe_localvalue id loc =
          match Ident_env.identifier_of_loc env loc with
          | None -> Some (LocalDefinition id, loc)
          | Some _ -> None
        in
        let () =
          match pat_desc with
#if OCAML_VERSION >= (5, 2, 0)
          | Tpat_var (id, loc, _uid, _) -> (
#else
          | Tpat_var (id, loc, _, _) -> (
#endif
              match maybe_localvalue id loc.loc with
              | Some x -> poses := x :: !poses
              | None -> ())
#if OCAML_VERSION >= (5, 2, 0)
          | Tpat_alias (_, id, loc, _uid, _, _) -> (
#else
          | Tpat_alias (_, id, loc, _, _) -> (
#endif
              match maybe_localvalue id loc.loc with
              | Some x -> poses := x :: !poses
              | None -> ())
          | _ -> ()
        in
        ()
    | _ -> ()

  let module_binding env poses = function
    | { Typedtree.mb_id = Some id; mb_loc; _ } when not mb_loc.loc_ghost -> (
        match Ident_env.identifier_of_loc env mb_loc with
        | None -> poses := (LocalDefinition id, mb_loc) :: !poses
        | Some _ -> ())
    | _ -> ()

  let module_expr poses mod_expr =
    match mod_expr with
    | { Typedtree.mod_desc = Tmod_ident (p, _); mod_loc; _ }
      when not mod_loc.loc_ghost ->
        poses := (Module p, mod_loc) :: !poses
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
  let iter = Tast_iterator.default_iterator in
  let module_expr iterator mod_expr =
    Analysis.module_expr poses mod_expr;
    iter.module_expr iterator mod_expr
  in
  let expr iterator e =
    Analysis.expr poses e;
    iter.expr iterator e
  in
  let pat iterator e =
    Analysis.pat env poses e;
    iter.pat iterator e
  in
  let typ iterator ctyp_expr =
    Analysis.core_type poses ctyp_expr;
    iter.typ iterator ctyp_expr
  in
  let module_type iterator mty =
    Analysis.module_type poses mty;
    iter.module_type iterator mty
  in
  let module_binding iterator mb =
    Analysis.module_binding env poses mb;
    iter.module_binding iterator mb
  in
  let iterator =
    {
      iter with
      expr;
      pat;
      module_expr;
      typ;
      module_type;
      module_binding;
    }
  in
  iterator.structure iterator structure;
  !poses

#else

#endif
