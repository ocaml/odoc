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
open Parsetree
open Types
open Typedtree

module OCamlPath = Path

open DocOckPaths
open DocOckTypes
open DocOckAttrs

module Env = DocOckIdentEnv

let opt_map f = function
  | None -> None
  | Some x -> Some (f x)

let opt_iter f = function
  | None -> ()
  | Some x -> f x

let parenthesise name =
  match name with
  | "asr" | "land" | "lnot" | "lor" | "lsl" | "lsr"
  | "lxor" | "mod" -> "(" ^ name ^ ")"
  | _ ->
    if (String.length name > 0) then
      match name.[0] with
      | 'a' .. 'z' | '\223' .. '\246' | '\248' .. '\255' | '_'
      | 'A' .. 'Z' | '\192' .. '\214' | '\216' .. '\222' -> name
      | _ -> "(" ^ name ^ ")"
    else name

let read_core_type env ctyp =
  DocOckCmi.read_type_expr env ctyp.ctyp_type

let rec read_pattern env parent doc pat =
  let open Signature in
    match pat.pat_desc with
    | Tpat_any -> []
    | Tpat_var(id, _) ->
        let open Value in
        let name = parenthesise (Ident.name id) in
        let id = Identifier.Value(parent, name) in
          DocOckCmi.mark_type_expr pat.pat_type;
          let type_ = DocOckCmi.read_type_expr env pat.pat_type in
            [Value {id; doc; type_}]
    | Tpat_alias(pat, id, _) ->
        let open Value in
        let name = parenthesise (Ident.name id) in
        let id = Identifier.Value(parent, name) in
          DocOckCmi.mark_type_expr pat.pat_type;
          let type_ = DocOckCmi.read_type_expr env pat.pat_type in
            Value {id; doc; type_} :: read_pattern env parent doc pat
    | Tpat_constant _ -> []
    | Tpat_tuple pats ->
        List.concat (List.map (read_pattern env parent doc) pats)
    | Tpat_construct(_, _, pats) ->
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

let read_value_binding env parent vb =
  let open Signature in
  let container = Identifier.parent_of_signature parent in
  let doc = read_attributes container parent vb.vb_attributes in
    read_pattern env parent doc vb.vb_pat

let read_value_bindings env parent vbs =
  let container = Identifier.parent_of_signature parent in
  let items =
    List.fold_left
      (fun acc vb ->
         let open Signature in
         let comments = read_comments container vb.vb_attributes in
         let comments = List.map (fun com -> Comment com) comments in
         let vb = read_value_binding env parent vb in
          List.rev_append vb (List.rev_append comments acc))
      [] vbs
  in
    List.rev items

let read_type_extension env parent tyext =
  let open Extension in
  let type_path = Env.Path.read_type env tyext.tyext_path in
  let container = Identifier.parent_of_signature parent in
  let doc = read_attributes container parent tyext.tyext_attributes in
  let type_params =
    List.map (fun (ctyp, _) -> ctyp.ctyp_type) tyext.tyext_params
  in
  let constructors =
    List.map (fun ext -> ext.ext_type) tyext.tyext_constructors
  in
  let type_params =
    DocOckCmi.mark_type_extension type_params constructors
  in
  let type_params =
    List.map
      (DocOckCmi.read_type_parameter false Variance.null)
      type_params
  in
  let private_ = (tyext.tyext_private = Private) in
  let constructors =
    List.map
      (fun ext ->
         DocOckCmi.read_extension_constructor
           env parent ext.ext_id ext.ext_type)
      tyext.tyext_constructors
  in
    { type_path; doc; type_params; private_; constructors; }

let rec read_class_type_field env parent ctf =
  let open ClassSignature in
  let container = Identifier.parent_of_class_signature parent in
  let doc = read_attributes container parent ctf.ctf_attributes in
  match ctf.ctf_desc with
  | Tctf_val(name, mutable_, virtual_, typ) ->
      let open InstanceVariable in
      let name = parenthesise name in
      let id = Identifier.InstanceVariable(parent, name) in
      let mutable_ = (mutable_ = Mutable) in
      let virtual_ = (virtual_ = Virtual) in
      let type_ = read_core_type env typ in
        Some (InstanceVariable {id; doc; mutable_; virtual_; type_})
  | Tctf_method(name, private_, virtual_, typ) ->
      let open Method in
      let name = parenthesise name in
      let id = Identifier.Method(parent, name) in
      let private_ = (private_ = Private) in
      let virtual_ = (virtual_ = Virtual) in
      let type_ = read_core_type env typ in
        Some (Method {id; doc; private_; virtual_; type_})
  | Tctf_constraint(typ1, typ2) -> None
  | Tctf_inherit cltyp ->
      Some (Inherit (read_class_signature env parent [] cltyp))
  | Tctf_attribute attr ->
      match read_comment container attr with
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
          DocOckCmi.read_self_type env csig.csig_self.ctyp_type
        in
        let constraints =
          DocOckCmi.read_type_constraints env params
        in
        let constraints =
          List.map
            (fun (typ1, typ2) -> Constraint(typ1, typ2))
            constraints
        in
        let items =
          List.fold_left
            (fun rest item ->
               match read_class_type_field env parent item with
               | None -> rest
               | Some item -> item :: rest)
            [] csig.csig_fields
        in
        let items = constraints @ List.rev items in
          Signature {self; items}
    | Tcty_arrow _ -> assert false

let rec read_class_type env parent params cty =
  let open Class in
  match cty.cltyp_desc with
  | Tcty_constr _ | Tcty_signature _ ->
      ClassType (read_class_signature env parent params cty)
  | Tcty_arrow(lbl, arg, res) ->
      let lbl = DocOckCmi.read_label lbl in
      let arg = read_core_type env arg in
      let res = read_class_type env parent params res in
        Arrow(lbl, arg, res)

let rec read_class_field env parent cf =
  let open ClassSignature in
  let container = Identifier.parent_of_class_signature parent in
  let doc = read_attributes container parent cf.cf_attributes in
  match cf.cf_desc with
  | Tcf_val({txt = name}, mutable_, _, kind, _) ->
      let open InstanceVariable in
      let name = parenthesise name in
      let id = Identifier.InstanceVariable(parent, name) in
      let mutable_ = (mutable_ = Mutable) in
      let virtual_, type_ =
        match kind with
        | Tcfk_virtual typ ->
            true, read_core_type env typ
        | Tcfk_concrete(_, expr) ->
            false, DocOckCmi.read_type_expr env expr.exp_type
      in
        Some (InstanceVariable {id; doc; mutable_; virtual_; type_})
  | Tcf_method({txt = name}, private_, kind) ->
      let open Method in
      let name = parenthesise name in
      let id = Identifier.Method(parent, name) in
      let private_ = (private_ = Private) in
      let virtual_, type_ =
        match kind with
        | Tcfk_virtual typ ->
            true, read_core_type env typ
        | Tcfk_concrete(_, expr) ->
            false, DocOckCmi.read_type_expr env expr.exp_type
      in
        Some (Method {id; doc; private_; virtual_; type_})
  | Tcf_constraint(typ1, typ2) -> None
  | Tcf_inherit(_, cl, _, _, _) ->
      Some (Inherit (read_class_structure env parent [] cl))
  | Tcf_initializer _ -> None
  | Tcf_attribute attr ->
      match read_comment container attr with
      | None -> None
      | Some doc -> Some (Comment doc)

and read_class_structure env parent params cl =
  let open ClassType in
    match cl.cl_desc with
    | Tcl_ident _ | Tcl_apply _ ->
        DocOckCmi.read_class_signature env parent params cl.cl_type
    | Tcl_structure cstr ->
        let open ClassSignature in
        let self = DocOckCmi.read_self_type env cstr.cstr_self.pat_type in
        let constraints =
          DocOckCmi.read_type_constraints env params
        in
        let constraints =
          List.map
            (fun (typ1, typ2) -> Constraint(typ1, typ2))
            constraints
        in
        let items =
          List.fold_left
            (fun rest item ->
               match read_class_field env parent item with
               | None -> rest
               | Some item -> item :: rest)
            [] cstr.cstr_fields
        in
        let items = constraints @ List.rev items in
          Signature {self; items}
    | Tcl_fun _ -> assert false
    | Tcl_let(_, _, _, cl) -> read_class_structure env parent params cl
    | Tcl_constraint(cl, None, _, _, _) -> read_class_structure env parent params cl
    | Tcl_constraint(_, Some cltyp, _, _, _) ->
        read_class_signature env parent params cltyp

let rec read_class_expr env parent params cl =
  let open Class in
  match cl.cl_desc with
  | Tcl_ident _ | Tcl_apply _ ->
      DocOckCmi.read_class_type env parent params cl.cl_type
  | Tcl_structure _ ->
      ClassType (read_class_structure env parent params cl)
  | Tcl_fun(lbl, arg, _, res, _) ->
      let lbl = DocOckCmi.read_label lbl in
      let arg = DocOckCmi.read_type_expr env arg.pat_type in
      let res = read_class_expr env parent params res in
        Arrow(lbl, arg, res)
  | Tcl_let(_, _, _, cl) ->
      read_class_expr env parent params cl
  | Tcl_constraint(cl, None, _, _, _) ->
      read_class_expr env parent params cl
  | Tcl_constraint(_, Some cltyp, _, _, _) ->
      read_class_type env parent params cltyp

let read_class_declaration env parent cld =
  let open Class in
  let name = parenthesise (Ident.name cld.ci_id_class) in
  let id = Identifier.Class(parent, name) in
  let container = Identifier.parent_of_signature parent in
  let doc = read_attributes container id cld.ci_attributes in
    DocOckCmi.mark_class_declaration cld.ci_decl;
    let virtual_ = (cld.ci_virt = Virtual) in
    let clparams =
      List.map (fun (ctyp, _) -> ctyp.ctyp_type) cld.ci_params
    in
    let params =
      List.map
        (DocOckCmi.read_type_parameter false Variance.null)
        clparams
    in
    let type_ = read_class_expr env id clparams cld.ci_expr in
      { id; doc; virtual_; params; type_; expansion = None }

let read_class_declarations env parent clds =
  let container = Identifier.parent_of_signature parent in
  let items =
    List.fold_left
      (fun acc cld ->
         let open Signature in
         let comments = read_comments container cld.ci_attributes in
         let comments = List.map (fun com -> Comment com) comments in
         let cld = read_class_declaration env parent cld in
           (Class cld) :: (List.rev_append comments acc))
      [] clds
  in
    List.rev items

let rec read_module_expr env parent pos mexpr =
  let open ModuleType in
    match mexpr.mod_desc with
    | Tmod_ident _ ->
        DocOckCmi.read_module_type env parent pos mexpr.mod_type
    | Tmod_structure str -> Signature (read_structure env parent str)
    | Tmod_functor(id, _, arg, res) ->
        let arg =
          match arg with
          | None -> None
          | Some arg ->
              let name = parenthesise (Ident.name id) in
              let id = Identifier.Argument(parent, pos, name) in
              let arg = DocOckCmti.read_module_type env id 1 arg in
              let expansion =
                match arg with
                | Signature _ -> Some Module.AlreadyASig
                | _ -> None
              in
                Some { FunctorArgument. id; expr = arg; expansion }
        in
        let env = Env.add_argument parent pos id env in
        let res = read_module_expr env parent (pos + 1) res in
          Functor(arg, res)
    | Tmod_apply _ ->
        DocOckCmi.read_module_type env parent pos mexpr.mod_type
    | Tmod_constraint(_, _, Tmodtype_explicit mty, _) ->
        DocOckCmti.read_module_type env parent pos mty
    | Tmod_constraint(mexpr, _, Tmodtype_implicit, _) ->
        read_module_expr env parent pos mexpr
    | Tmod_unpack(_, mty) ->
        DocOckCmi.read_module_type env parent pos mty

and unwrap_module_expr_desc = function
  | Tmod_constraint(mexpr, _, Tmodtype_implicit, _) ->
      unwrap_module_expr_desc mexpr.mod_desc
  | desc -> desc

and read_module_binding env parent mb =
  let open Module in
  let name = parenthesise (Ident.name mb.mb_id) in
  let id = Identifier.Module(parent, name) in
  let container = Identifier.parent_of_signature parent in
  let doc = read_attributes container id mb.mb_attributes in
  let canonical =
    let open Documentation in
    match doc with
    | Ok { tags; _ } ->
      begin match List.find (function Canonical _ -> true | _ -> false) tags with
      | exception Not_found -> None
      | Canonical(p, r) -> Some (p, r)
      | _ -> None
      end
    | _ -> None
  in
  let type_ =
    match unwrap_module_expr_desc mb.mb_expr.mod_desc with
    | Tmod_ident(p, _) -> Alias (Env.Path.read_module env p)
    | _ -> ModuleType (read_module_expr env id 1 mb.mb_expr)
  in
  let hidden =
    match canonical with
    | Some _ -> false
    | None -> contains_double_underscore (Ident.name mb.mb_id)
  in
  let expansion =
    match type_ with
    | ModuleType (ModuleType.Signature _) -> Some AlreadyASig
    | _ -> None
  in
    {id; doc; type_; expansion; canonical; hidden; display_type = None}

and read_module_bindings env parent mbs =
  let container = Identifier.parent_of_signature parent in
  let items =
    List.fold_left
      (fun acc mb ->
         let open Signature in
         let comments = read_comments container mb.mb_attributes in
         let comments = List.map (fun com -> Comment com) comments in
         let mb = read_module_binding env parent mb in
           (Module mb) :: (List.rev_append comments acc))
      [] mbs
  in
    List.rev items

and read_structure_item env parent item =
  let open Signature in
    match item.str_desc with
    | Tstr_eval _ -> []
    | Tstr_value(_, vbs) ->
        read_value_bindings env parent vbs
    | Tstr_primitive vd ->
        [DocOckCmti.read_value_description env parent vd]
    | Tstr_type (_rec_flag, decls) -> (* TODO: handle rec_flag *)
        DocOckCmti.read_type_declarations env parent decls
    | Tstr_typext tyext ->
        [TypExt (read_type_extension env parent tyext)]
    | Tstr_exception ext ->
        let ext =
          DocOckCmi.read_exception env parent ext.ext_id ext.ext_type
        in
          [Exception ext]
    | Tstr_module mb ->
        [Module (read_module_binding env parent mb)]
    | Tstr_recmodule mbs ->
        read_module_bindings env parent mbs
    | Tstr_modtype mtd ->
        [ModuleType (DocOckCmti.read_module_type_declaration env parent mtd)]
    | Tstr_open _ -> []
    | Tstr_include incl ->
        [Include (read_include env parent incl)]
    | Tstr_class cls ->
        let cls = List.map (fun (cl, _) -> cl) cls in
          read_class_declarations env parent cls
    | Tstr_class_type cltyps ->
        let cltyps = List.map (fun (_, _, clty) -> clty) cltyps in
          DocOckCmti.read_class_type_declarations env parent cltyps
    | Tstr_attribute attr ->
        let container = Identifier.parent_of_signature parent in
          match read_comment container attr with
          | None -> []
          | Some doc -> [Comment doc]

and read_include env parent incl =
  let open Include in
  let container = Identifier.parent_of_signature parent in
  let doc = read_attributes container parent incl.incl_attributes in
  let decl =
    let open Module in
    match unwrap_module_expr_desc incl.incl_mod.mod_desc with
    | Tmod_ident(p, _) -> Alias (Env.Path.read_module env p)
    | _ -> ModuleType (read_module_expr env parent 1 incl.incl_mod)
  in
  let content = DocOckCmi.read_signature env parent incl.incl_type in
  let expansion = { content; resolved = false } in
    {parent; doc; decl; expansion}

and read_structure env parent str =
  let env = Env.add_structure_tree_items parent str env in
  let items =
    List.fold_left
      (fun items item ->
         List.rev_append (read_structure_item env parent item) items)
      [] str.str_items
  in
    List.rev items

let read_implementation root name impl =
  let open Module in
  let id = Identifier.Root(root, name) in
  let items = read_structure Env.empty id impl in
  let doc, items =
    let open Signature in
    let open Documentation in
    match items with
    | Comment (Documentation doc) :: items -> doc, items
    | _ -> empty, items
  in
    (id, doc, items)
