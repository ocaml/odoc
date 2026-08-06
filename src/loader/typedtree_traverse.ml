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
#if defined OXCAML
      | Texp_ident (p, _, _, _, _, _) ->
#else
      | Texp_ident (p, _, _) ->
#endif
          poses := (Value p, exp_loc) :: !poses
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
#if defined OXCAML
          | Tpat_var (id, loc, _uid, _, _) -> (
#elif OCAML_VERSION >= (5, 2, 0)
          | Tpat_var (id, loc, _uid) -> (
#else
          | Tpat_var (id, loc) -> (
#endif
              match maybe_localvalue id loc.loc with
              | Some x -> poses := x :: !poses
              | None -> ())
#if defined OXCAML
          | Tpat_alias (_, id, loc, _uid, _, _, _) -> (
#elif OCAML_VERSION >= (5, 4, 0)
          | Tpat_alias (_, id, loc, _uid, _ty) -> (
#elif OCAML_VERSION >= (5, 2, 0)
          | Tpat_alias (_, id, loc, _uid) -> (
#else
          | Tpat_alias (_, id, loc) -> (
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

(* Honor [@merlin.hide] attribute to prevent collecting occurences from
   generated code with invalid non ghost locations (see ocaml/odoc#1456).
   
   The [@merlin.hide] attribute is used by Merlin to hide generated code.
   It is automatically added by ppx_deriving, and it hides a whole
   subtree (as opposed to a single node like ghost locations), making it
   a more reliable source of truth to detect generated code.

   Mirrors Merlin's [iter_only_visible] (src/analysis/ast_iterators.ml) and
   Ppxlib.Location_check, which check [@merlin.hide] on every attribute-bearing
   node. Keep the guarded nodes below in sync: we cover the kinds odoc collects
   occurrences from, plus their recursion parents. *)
let not_hidden attrs =
  not
    (List.exists
       (fun (a : Parsetree.attribute) -> a.attr_name.txt = "merlin.hide")
       attrs)

let of_cmt env structure =
  let poses = ref [] in
  let iter = Tast_iterator.default_iterator in
  (* Nodes that carry occurrences: analyse then recurse, unless hidden. *)
  let module_expr iterator ({ Typedtree.mod_attributes; _ } as mod_expr) =
    if not_hidden mod_attributes then (
      Analysis.module_expr poses mod_expr;
      iter.module_expr iterator mod_expr)
  in
  let expr iterator ({ Typedtree.exp_attributes; _ } as e) =
    if not_hidden exp_attributes then (
      Analysis.expr poses e;
      iter.expr iterator e)
  in
  let pat iterator (type k) (p : k Typedtree.general_pattern) =
    if not_hidden p.pat_attributes then (
      Analysis.pat env poses p;
      iter.pat iterator p)
  in
  let typ iterator ({ Typedtree.ctyp_attributes; _ } as ctyp_expr) =
    if not_hidden ctyp_attributes then (
      Analysis.core_type poses ctyp_expr;
      iter.typ iterator ctyp_expr)
  in
  let module_type iterator ({ Typedtree.mty_attributes; _ } as mty) =
    if not_hidden mty_attributes then (
      Analysis.module_type poses mty;
      iter.module_type iterator mty)
  in
  let module_binding iterator ({ Typedtree.mb_attributes; _ } as mb) =
    if not_hidden mb_attributes then (
      Analysis.module_binding env poses mb;
      iter.module_binding iterator mb)
  in
  (* Recursion parents (no occurrence of their own): prune when hidden. *)
  let value_binding iterator ({ Typedtree.vb_attributes; _ } as vb) =
    if not_hidden vb_attributes then iter.value_binding iterator vb
  in
  let value_description iterator ({ Typedtree.val_attributes; _ } as vd) =
    if not_hidden val_attributes then iter.value_description iterator vd
  in
  let type_declaration iterator ({ Typedtree.typ_attributes; _ } as td) =
    if not_hidden typ_attributes then iter.type_declaration iterator td
  in
  let type_extension iterator ({ Typedtree.tyext_attributes; _ } as te) =
    if not_hidden tyext_attributes then iter.type_extension iterator te
  in
  let type_exception iterator ({ Typedtree.tyexn_attributes; _ } as te) =
    if not_hidden tyexn_attributes then iter.type_exception iterator te
  in
  let extension_constructor iterator
      ({ Typedtree.ext_attributes; _ } as ec) =
    if not_hidden ext_attributes then iter.extension_constructor iterator ec
  in
  let module_declaration iterator ({ Typedtree.md_attributes; _ } as md) =
    if not_hidden md_attributes then iter.module_declaration iterator md
  in
  let module_type_declaration iterator
      ({ Typedtree.mtd_attributes; _ } as mtd) =
    if not_hidden mtd_attributes then iter.module_type_declaration iterator mtd
  in
  let open_declaration iterator ({ Typedtree.open_attributes; _ } as od) =
    if not_hidden open_attributes then iter.open_declaration iterator od
  in
  let open_description iterator ({ Typedtree.open_attributes; _ } as od) =
    if not_hidden open_attributes then iter.open_description iterator od
  in
  (* The stock [Tast_iterator] has no [include_declaration]/[include_description]
     field: includes are dispatched through [structure_item]/[signature_item],
     which is where a deriver's [include ... [@@merlin.hide]] wrapper lands. *)
  let structure_item iterator str_item =
    let visible =
      match str_item.Typedtree.str_desc with
      | Tstr_include { incl_attributes; _ } -> not_hidden incl_attributes
      | _ -> true
    in
    if visible then iter.structure_item iterator str_item
  in
  let signature_item iterator sig_item =
    let visible =
      match sig_item.Typedtree.sig_desc with
#if defined OXCAML
      | Tsig_include ({ incl_attributes; _ }, _)
#else
      | Tsig_include { incl_attributes; _ }
#endif
        -> not_hidden incl_attributes
      | _ -> true
    in
    if visible then iter.signature_item iterator sig_item
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
      value_binding;
      value_description;
      type_declaration;
      type_extension;
      type_exception;
      extension_constructor;
      module_declaration;
      module_type_declaration;
      open_declaration;
      open_description;
      structure_item;
      signature_item;
    }
  in
  iterator.structure iterator structure;
  !poses

#else

#endif
