(*
 * Copyright (c) 2014 Leo White <lpw25@cl.cam.ac.uk>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)


open Asttypes
open Typedtree

module OCamlPath = Path

open Odoc_model.Paths
open Odoc_model.Lang

module Env = Ident_env

let read_core_type env ctyp =
  Cmi.read_type_expr env ctyp.ctyp_type

let rec read_pattern env parent doc pat =
  let locs _id = None in
  let open Signature in
    match pat.pat_desc with
    | Tpat_any -> []
#if OCAML_VERSION < (5,2,0)
    | Tpat_var(id, _) ->
#else
    | Tpat_var(id,_,_uid) ->
#endif
        let open Value in
        let id = Env.find_value_identifier env id in
          Cmi.mark_type_expr pat.pat_type;
          let type_ = Cmi.read_type_expr env pat.pat_type in
          let value = Abstract in
          [Value {id; locs = locs id; doc; type_; value}]
#if OCAML_VERSION < (5,2, 0)
    | Tpat_alias(pat, id, _) ->
#else
    | Tpat_alias(pat, id, _,_) ->
#endif
        let open Value in
        let id = Env.find_value_identifier env id in
          Cmi.mark_type_expr pat.pat_type;
          let type_ = Cmi.read_type_expr env pat.pat_type in
          let value = Abstract in
          Value {id; locs = locs id; doc; type_; value} :: read_pattern env parent doc pat
    | Tpat_constant _ -> []
    | Tpat_tuple pats ->
        List.concat (List.map (read_pattern env parent doc) pats)
#if OCAML_VERSION < (4, 13, 0)
    | Tpat_construct(_, _, pats) ->
#else
    | Tpat_construct(_,_,pats,_) ->
#endif
        List.concat (List.map (read_pattern env parent doc) pats)
    | Tpat_variant(_, None, _) -> []
    | Tpat_variant(_, Some pat, _) ->
        read_pattern env parent doc pat
    | Tpat_record(pats, _) ->
        List.concat
          (List.map
             (fun (_, _, pat) -> read_pattern env parent doc pat)
          pats)
    | Tpat_array pats ->
        List.concat (List.map (read_pattern env parent doc) pats)
    | Tpat_or(pat, _, _) ->
        read_pattern env parent doc pat
    | Tpat_lazy pat ->
        read_pattern env parent doc pat
#if OCAML_VERSION >= (4,8,0) && OCAML_VERSION < (4,11,0)
    | Tpat_exception pat ->
        read_pattern env parent doc pat
#endif

let read_value_binding env parent vb =
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached_no_tag container vb.vb_attributes in
    read_pattern env parent doc vb.vb_pat

let read_value_bindings env parent vbs =
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let items =
    List.fold_left
      (fun acc vb ->
         let open Signature in
        let comments =
          Doc_attr.standalone_multiple container vb.vb_attributes in
         let comments = List.map (fun com -> Comment com) comments in
         let vb = read_value_binding env parent vb in
          List.rev_append vb (List.rev_append comments acc))
      [] vbs
  in
    List.rev items

let read_type_extension env parent tyext =
  let open Extension in
  let type_path = Env.Path.read_type env tyext.tyext_path in
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached_no_tag container tyext.tyext_attributes in
  let type_params =
    List.map (fun (ctyp, _) -> ctyp.ctyp_type) tyext.tyext_params
  in
  let constructors =
    List.map (fun ext -> ext.ext_type) tyext.tyext_constructors
  in
  let type_params =
    Cmi.mark_type_extension type_params constructors
  in
  let type_params =
    List.map
      (Cmi.read_type_parameter false Types.Variance.null)
      type_params
  in
  let private_ = (tyext.tyext_private = Private) in
  let constructors =
    List.map
      (fun ext ->
         Cmi.read_extension_constructor
           env parent ext.ext_id ext.ext_type)
      tyext.tyext_constructors
  in
  { parent; type_path; doc; type_params; private_; constructors; }

(** Make a standalone comment out of a comment attached to an item that isn't
    rendered. For example, [constraint] items are read separately and not
    associated with their comment. *)
let mk_class_comment = function
  | [] -> None
  | doc -> Some (ClassSignature.Comment (`Docs doc))

let rec read_class_type_field env parent ctf =
  let open ClassSignature in
  let open Odoc_model.Names in
  let container = (parent : Identifier.ClassSignature.t :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached_no_tag container ctf.ctf_attributes in
  match ctf.ctf_desc with
  | Tctf_val(name, mutable_, virtual_, typ) ->
      let open InstanceVariable in
      let id = Identifier.Mk.instance_variable(parent, InstanceVariableName.make_std name) in
      let mutable_ = (mutable_ = Mutable) in
      let virtual_ = (virtual_ = Virtual) in
      let type_ = read_core_type env typ in
        Some (InstanceVariable {id; doc; mutable_; virtual_; type_})
  | Tctf_method(name, private_, virtual_, typ) ->
      let open Method in
      let id = Identifier.Mk.method_(parent, MethodName.make_std name) in
      let private_ = (private_ = Private) in
      let virtual_ = (virtual_ = Virtual) in
      let type_ = read_core_type env typ in
        Some (Method {id; doc; private_; virtual_; type_})
  | Tctf_constraint(_, _) -> mk_class_comment doc
  | Tctf_inherit cltyp ->
      let expr = read_class_signature env parent [] cltyp in
      Some (Inherit {Inherit.expr; doc})
  | Tctf_attribute attr ->
      match Doc_attr.standalone container attr with
      | None -> None
      | Some doc -> Some (Comment doc)

and read_class_signature env parent params cltyp =
  let open ClassType in
    match cltyp.cltyp_desc with
    | Tcty_constr(p, _, params) ->
        let p = Env.Path.read_class_type env p in
        let params = List.map (read_core_type env) params in
          Constr(p, params)
    | Tcty_signature csig ->
        let open ClassSignature in
        let self =
          Cmi.read_self_type csig.csig_self.ctyp_type
        in
        let constraints = Cmi.read_class_constraints env params in
        let items =
          List.fold_left
            (fun rest item ->
               match read_class_type_field env parent item with
               | None -> rest
               | Some item -> item :: rest)
            [] csig.csig_fields
        in
        let items = constraints @ List.rev items in
        let items, (doc, doc_post) = Doc_attr.extract_top_comment_class items in
        let items =
          match doc_post with
          | [] -> items
          | _ -> Comment (`Docs doc_post) :: items
        in
        Signature {self; items; doc}

    | Tcty_arrow _ -> assert false
#if OCAML_VERSION >= (4,6,0)
    | Tcty_open _ -> assert false
#endif

let rec read_class_type env parent params cty =
  let open Class in
  match cty.cltyp_desc with
  | Tcty_constr _ | Tcty_signature _ ->
      ClassType (read_class_signature env parent params cty)
  | Tcty_arrow(lbl, arg, res) ->
      let lbl = Cmi.read_label lbl in
      let arg = read_core_type env arg in
      let res = read_class_type env parent params res in
        Arrow(lbl, arg, res)
#if OCAML_VERSION >= (4,8,0)
  | Tcty_open (_, cty) -> read_class_type env parent params cty
#elif OCAML_VERSION >= (4,6,0)
  | Tcty_open (_, _, _, _, cty) -> read_class_type env parent params cty
#endif


let rec read_class_field env parent cf =
  let open ClassSignature in
  let open Odoc_model.Names in
  let container = (parent : Identifier.ClassSignature.t :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached_no_tag container (cf.cf_attributes) in
  match cf.cf_desc with
  | Tcf_val({txt = name; _}, mutable_, _, kind, _) ->
      let open InstanceVariable in
      let id = Identifier.Mk.instance_variable(parent, InstanceVariableName.make_std name) in
      let mutable_ = (mutable_ = Mutable) in
      let virtual_, type_ =
        match kind with
        | Tcfk_virtual typ ->
            true, read_core_type env typ
        | Tcfk_concrete(_, expr) ->
            false, Cmi.read_type_expr env expr.exp_type
      in
        Some (InstanceVariable {id; doc; mutable_; virtual_; type_})
  | Tcf_method({txt = name; _}, private_, kind) ->
      let open Method in
      let id = Identifier.Mk.method_(parent, MethodName.make_std name) in
      let private_ = (private_ = Private) in
      let virtual_, type_ =
        match kind with
        | Tcfk_virtual typ ->
            true, read_core_type env typ
        | Tcfk_concrete(_, expr) ->
            (* Types of concrete methods in class implementation begin
               with the object as first (implicit) argument, so we
               must keep only the type after the first arrow. *)
            let type_ =
              match Cmi.read_type_expr env expr.exp_type with
              | Arrow (_, _, t) -> t
              | t -> t
            in
            false, type_
      in
        Some (Method {id; doc; private_; virtual_; type_})
  | Tcf_constraint(_, _) -> mk_class_comment doc
  | Tcf_inherit(_, cl, _, _, _) ->
      let expr = read_class_structure env parent [] cl in
      Some (Inherit {Inherit.expr; doc})
  | Tcf_initializer _ -> mk_class_comment doc
  | Tcf_attribute attr ->
      match Doc_attr.standalone container attr with
      | None -> None
      | Some doc -> Some (Comment doc)

and read_class_structure env parent params cl =
  let open ClassType in
    match cl.cl_desc with
    | Tcl_ident _ | Tcl_apply _ ->
        Cmi.read_class_signature env parent params cl.cl_type
    | Tcl_structure cstr ->
        let open ClassSignature in
        let self = Cmi.read_self_type cstr.cstr_self.pat_type in
        let constraints = Cmi.read_class_constraints env params in
        let items =
          List.fold_left
            (fun rest item ->
               match read_class_field env parent item with
               | None -> rest
               | Some item -> item :: rest)
            [] cstr.cstr_fields
        in
        let items = constraints @ List.rev items in
        let items, (doc, doc_post) = Doc_attr.extract_top_comment_class items in
        let items =
          match doc_post with
          | [] -> items
          | _ -> Comment (`Docs doc_post) :: items
        in
        Signature {self; items; doc}
    | Tcl_fun _ -> assert false
    | Tcl_let(_, _, _, cl) -> read_class_structure env parent params cl
    | Tcl_constraint(cl, None, _, _, _) -> read_class_structure env parent params cl
    | Tcl_constraint(_, Some cltyp, _, _, _) ->
        read_class_signature env parent params cltyp
#if OCAML_VERSION >= (4,8,0)
    | Tcl_open (_, cl) -> read_class_structure env parent params cl
#elif OCAML_VERSION >= (4,6,0)
    | Tcl_open (_, _, _, _, cl) -> read_class_structure env parent params cl
#endif


let rec read_class_expr env parent params cl =
  let open Class in
  match cl.cl_desc with
  | Tcl_ident _ | Tcl_apply _ ->
      Cmi.read_class_type env parent params cl.cl_type
  | Tcl_structure _ ->
      ClassType (read_class_structure env parent params cl)
  | Tcl_fun(lbl, arg, _, res, _) ->
      let lbl = Cmi.read_label lbl in
      let arg = Cmi.read_type_expr env arg.pat_type in
      let res = read_class_expr env parent params res in
        Arrow(lbl, arg, res)
  | Tcl_let(_, _, _, cl) ->
      read_class_expr env parent params cl
  | Tcl_constraint(cl, None, _, _, _) ->
      read_class_expr env parent params cl
  | Tcl_constraint(_, Some cltyp, _, _, _) ->
      read_class_type env parent params cltyp
#if OCAML_VERSION >= (4,8,0)
    | Tcl_open (_, cl) -> read_class_expr env parent params cl
#elif OCAML_VERSION >= (4,6,0)
    | Tcl_open (_, _, _, _, cl) -> read_class_expr env parent params cl
#endif

let read_class_declaration env parent cld =
  let open Class in
  let id = Env.find_class_identifier env cld.ci_id_class in
  let locs = None in
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached_no_tag container cld.ci_attributes in
    Cmi.mark_class_declaration cld.ci_decl;
    let virtual_ = (cld.ci_virt = Virtual) in
    let clparams =
      List.map (fun (ctyp, _) -> ctyp.ctyp_type) cld.ci_params
    in
    let params =
      List.map
        (Cmi.read_type_parameter false Types.Variance.null)
        clparams
    in
    let type_ = read_class_expr env (id :> Identifier.ClassSignature.t) clparams cld.ci_expr in
    { id; locs; doc; virtual_; params; type_; expansion = None }

let read_class_declarations env parent clds =
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let open Signature in
  List.fold_left begin fun (acc, recursive) cld ->
    let comments = Doc_attr.standalone_multiple container cld.ci_attributes in
    let comments = List.map (fun com -> Comment com) comments in
    let cld = read_class_declaration env parent cld in
    ((Class (recursive, cld))::(List.rev_append comments acc), And)
  end ([], Ordinary) clds
  |> fst
  |> List.rev

let rec read_module_expr env parent label_parent mexpr =
  let open ModuleType in
  let open Odoc_model.Names in
    match mexpr.mod_desc with
    | Tmod_ident _ ->
        Cmi.read_module_type env parent (Odoc_model.Compat.module_type mexpr.mod_type)
    | Tmod_structure str ->
        let sg, () = read_structure Odoc_model.Semantics.Expect_none env parent str in
        Signature sg
#if OCAML_VERSION >= (4,10,0)
    | Tmod_functor(parameter, res) ->
        let f_parameter, env =
          match parameter with
          | Unit -> FunctorParameter.Unit, env
          | Named (id_opt, _, arg) ->
              let id, env =
                match id_opt with
                | None -> Identifier.Mk.parameter (parent, Odoc_model.Names.ModuleName.make_std "_"), env
                | Some id -> let env = Env.add_parameter parent id (ModuleName.of_ident id) env in
                  Env.find_parameter_identifier env id, env
              in
              let arg = Cmti.read_module_type env (id :> Identifier.Signature.t) label_parent arg in

              Named { id; expr=arg }, env
          in
        let res = read_module_expr env (Identifier.Mk.result parent) label_parent res in
        Functor (f_parameter, res)
#else
    | Tmod_functor(id, _, arg, res) ->
        let new_env = Env.add_parameter parent id (ModuleName.of_ident id) env in
        let f_parameter =
          match arg with
          | None -> FunctorParameter.Unit
          | Some arg ->
              let id = Env.find_parameter_identifier new_env id in
              let arg = Cmti.read_module_type env (id :> Identifier.Signature.t) label_parent arg in
              Named { FunctorParameter. id; expr = arg; }
        in
        let res = read_module_expr new_env (Identifier.Mk.result parent) label_parent res in
        Functor(f_parameter, res)
#endif
    | Tmod_apply _ ->
        Cmi.read_module_type env parent (Odoc_model.Compat.module_type mexpr.mod_type)
#if OCAML_VERSION >= (5,1,0)
    | Tmod_apply_unit _ ->
        Cmi.read_module_type env parent (Odoc_model.Compat.module_type mexpr.mod_type)
#endif
    | Tmod_constraint(_, _, Tmodtype_explicit mty, _) ->
        Cmti.read_module_type env parent label_parent mty
    | Tmod_constraint(mexpr, _, Tmodtype_implicit, _) ->
        read_module_expr env parent label_parent mexpr
    | Tmod_unpack(_, mty) ->
        Cmi.read_module_type env parent (Odoc_model.Compat.module_type mty)
and unwrap_module_expr_desc = function
  | Tmod_constraint(mexpr, _, Tmodtype_implicit, _) ->
      unwrap_module_expr_desc mexpr.mod_desc
  | desc -> desc

(** Like [read_module_expr] but handle the canonical tag in the top-comment. *)
and read_module_expr_maybe_canonical env parent container ~canonical mexpr =
  let open ModuleType in
  match (canonical, mexpr.mod_desc) with
  | None, Tmod_structure str ->
      let sg, canonical =
        read_structure Odoc_model.Semantics.Expect_canonical env parent str
      in
      (Signature sg, canonical)
  | _ -> (read_module_expr env parent container mexpr, canonical)

and read_module_binding env parent mb =
  let open Module in
#if OCAML_VERSION >= (4,10,0)
      match mb.mb_id with
      | None -> None
      | Some id ->
        let id = Env.find_module_identifier env id in
#else
  let id = Env.find_module_identifier env mb.mb_id in
#endif
  let id = (id :> Identifier.Module.t) in
  let locs = None in
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc, canonical = Doc_attr.attached Odoc_model.Semantics.Expect_canonical container mb.mb_attributes in
  let type_, canonical =
    match unwrap_module_expr_desc mb.mb_expr.mod_desc with
    | Tmod_ident (p, _) -> (Alias (Env.Path.read_module env p, None), canonical)
    | _ ->
        let id = (id :> Identifier.Signature.t) in
        let expr, canonical =
          read_module_expr_maybe_canonical env id container ~canonical mb.mb_expr
        in
        (ModuleType expr, canonical)
  in
  let canonical = (canonical :> Path.Module.t option) in
  let hidden =
#if OCAML_VERSION >= (4,10,0)
    match canonical, mb.mb_id with
    | None, Some id -> Odoc_model.Root.contains_double_underscore (Ident.name id)
    | _, _ -> false
#else
    match canonical with
    | None -> Odoc_model.Root.contains_double_underscore (Ident.name mb.mb_id)
    | _ -> false
#endif
  in
  Some {id; locs; doc; type_; canonical; hidden; }

and read_module_bindings env parent mbs =
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t)
  in
  let open Signature in
  List.fold_left
    (fun (acc, recursive) mb ->
      let comments = Doc_attr.standalone_multiple container mb.mb_attributes in
      let comments = List.map (fun com -> Comment com) comments in
      match read_module_binding env parent mb with
      | Some mb ->
        ((Module (recursive, mb))::(List.rev_append comments acc), And)
      | None -> (acc, recursive))
    ([], Rec) mbs
  |> fst
  |> List.rev

and read_structure_item env parent item =
  let open Signature in
    match item.str_desc with
    | Tstr_eval _ -> []
    | Tstr_value(_, vbs) ->
        read_value_bindings env parent vbs
    | Tstr_primitive vd ->
        [Cmti.read_value_description env parent vd]
#if OCAML_VERSION < (4,3,0)
    | Tstr_type (decls) ->
      let rec_flag = Ordinary in
#else
    | Tstr_type (rec_flag, decls) ->
      let rec_flag =
        match rec_flag with
        | Recursive -> Ordinary
        | Nonrecursive -> Nonrec
      in
#endif
      Cmti.read_type_declarations env parent rec_flag decls
    | Tstr_typext tyext ->
        [TypExt (read_type_extension env parent tyext)]
    | Tstr_exception ext ->
        let ext =
#if OCAML_VERSION >= (4,8,0)
          Cmi.read_exception env parent ext.tyexn_constructor.ext_id ext.tyexn_constructor.ext_type
#else
          Cmi.read_exception env parent ext.ext_id ext.ext_type
#endif
        in
          [Exception ext]
    | Tstr_module mb -> begin
        match read_module_binding env parent mb with
        | Some mb ->
          [Module (Ordinary, mb)]
        | None -> []
        end
    | Tstr_recmodule mbs ->
        read_module_bindings env parent mbs
    | Tstr_modtype mtd ->
        [ModuleType (Cmti.read_module_type_declaration env parent mtd)]
    | Tstr_open o ->
        [Open (read_open env parent o)]
    | Tstr_include incl ->
        read_include env parent incl
    | Tstr_class cls ->
        let cls = List.map
#if OCAML_VERSION < (4,3,0)
          (* NOTE(@ostera): remember the virtual flag was removed post 4.02 *)
          (fun (cl, _, _) -> cl)
#else
          (fun (cl, _) -> cl)
#endif
          cls in
          read_class_declarations env parent cls
    | Tstr_class_type cltyps ->
        let cltyps = List.map (fun (_, _, clty) -> clty) cltyps in
          Cmti.read_class_type_declarations env parent cltyps
    | Tstr_attribute attr ->
      let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
          match Doc_attr.standalone container attr with
          | None -> []
          | Some doc -> [Comment doc]

and read_include env parent incl =
  let open Include in
  let loc = Doc_attr.read_location incl.incl_loc in
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc, status = Doc_attr.attached Odoc_model.Semantics.Expect_status container incl.incl_attributes in
  let decl_modty =
    match unwrap_module_expr_desc incl.incl_mod.mod_desc with
    | Tmod_ident(p, _) ->
      Some (ModuleType.U.TypeOf {t_desc = ModuleType.StructInclude (Env.Path.read_module env p); t_expansion=None })
    | _ ->
      let mty = read_module_expr env parent container incl.incl_mod in
      umty_of_mty mty
  in
  let content, shadowed = Cmi.read_signature_noenv env parent (Odoc_model.Compat.signature incl.incl_type) in
  let expansion = { content; shadowed; } in
  match decl_modty with
  | Some m ->
    let decl = ModuleType m in
    [Include {parent; doc; decl; expansion; status; strengthened=None; loc }]
  | _ ->
    content.items

and read_open env parent o =
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached_no_tag container o.open_attributes in
  #if OCAML_VERSION >= (4,8,0)
  let signature = o.open_bound_items in
  #else
  let signature = [] in
  #endif
  let expansion, _ = Cmi.read_signature_noenv env parent (Odoc_model.Compat.signature signature) in
  Open.{expansion; doc}

and read_structure :
      'tags. 'tags Odoc_model.Semantics.handle_internal_tags -> _ -> _ -> _ ->
      _ * 'tags =
 fun internal_tags env parent str ->
  let env = Env.add_structure_tree_items parent str env in
  let items, (doc, doc_post), tags =
    let classify item =
      match item.str_desc with
      | Tstr_open _ -> Some `Open
      | Tstr_attribute attr -> Some (`Attribute attr)
      | _ -> None
    in
    Doc_attr.extract_top_comment internal_tags ~classify parent str.str_items
  in
  let items =
    List.fold_left
      (fun items item ->
        List.rev_append (read_structure_item env parent item) items)
      [] items
    |> List.rev
  in
  match doc_post with
  | [] ->
    ({ Signature.items; compiled = false; doc }, tags)
  | _ ->
    ({ Signature.items = Comment (`Docs doc_post) :: items; compiled=false; doc }, tags)

let read_implementation root name impl =
  let id = Identifier.Mk.root (root, Odoc_model.Names.ModuleName.make_std name) in
  let sg, canonical =
    read_structure Odoc_model.Semantics.Expect_canonical (Env.empty ()) id impl
  in
  (id, sg, (canonical :> Odoc_model.Paths.Path.Module.t option))

let _ = Cmti.read_module_expr := read_module_expr
