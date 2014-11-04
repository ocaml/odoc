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

open DocOckPaths
open DocOckTypes
open DocOckAttrs

module Env = DocOckEnvironment

let opt_map f = function
  | None -> None
  | Some x -> Some (f x)

let opt_iter f = function
  | None -> ()
  | Some x -> f x

let read_label lbl =
  let open TypeExpr in
  if String.length lbl = 0 then None
  else if lbl.[0] = '?' then
    Some (Optional (String.sub lbl 1 (String.length lbl - 1)))
  else
    Some (Label (String.sub lbl 1 (String.length lbl - 1)))

let rec read_core_type env ctyp =
  let open TypeExpr in
    match ctyp.ctyp_desc with
    | Ttyp_any -> Any
    | Ttyp_var s -> Var s
    | Ttyp_arrow(lbl, arg, res) ->
        let lbl = read_label lbl in
        let arg = read_core_type env arg in
        let res = read_core_type env res in
          Arrow(lbl, arg, res)
    | Ttyp_tuple typs ->
        let typs = List.map (read_core_type env) typs in
          Tuple typs
    | Ttyp_constr(p, _, params) ->
        let p = Env.Path.read_type env p in
        let params = List.map (read_core_type env) params in
          Constr(p, params)
    | Ttyp_object(methods, closed) ->
        let open TypeExpr.Object in
        let methods =
          List.map
            (fun (name, _, typ) -> {name; type_ = read_core_type env typ})
            methods
        in
          Object {methods; open_ = (closed = Asttypes.Open)}
    | Ttyp_class(p, _, params) ->
        let p = Env.Path.read_class_type env p in
        let params = List.map (read_core_type env) params in
          Class(p, params)
    | Ttyp_alias(typ, var) ->
        let typ = read_core_type env typ in
          Alias(typ, var)
    | Ttyp_variant(fields, closed, present) ->
        let open TypeExpr.Variant in
        let elements =
          List.map
            (function
              | Ttag(name, _, const, args) ->
                  let args = List.map (read_core_type env) args in
                    Constructor(name, const, args)
              | Tinherit typ -> Type (read_core_type env typ))
            fields
        in
        let kind =
          if closed = Asttypes.Open then Open
          else match present with
            | None -> Fixed
            | Some names -> Closed names
        in
          Variant {kind; elements}
    | Ttyp_poly(vars, typ) ->
        Poly(vars, read_core_type env typ)
    | Ttyp_package {pack_path; pack_fields; _} ->
        let open TypeExpr.Package in
        let path = Env.Path.read_module_type env pack_path in
        let substitutions =
          List.map
            (fun (frag, typ) ->
               let frag = Env.Fragment.read_type frag.Location.txt in
               let typ = read_core_type env typ in
               (frag, typ))
            pack_fields
        in
          Package {path; substitutions}

let add_value_description parent vd env =
  let container = Identifier.container_of_signature parent in
  let env = add_attributes container vd.val_attributes env in
  let env = Env.add_value parent vd.val_id env in
    env

let read_value_description env parent vd =
  let open Signature in
  let id = Identifier.Value(parent, Ident.name vd.val_id) in
  let container = Identifier.container_of_signature parent in
  let doc = read_attributes env container vd.val_attributes in
  let type_ = read_core_type env vd.val_desc in
  match vd.val_prim with
  | [] -> Value {Value.id; doc; type_}
  | primitives -> External {External.id; doc; type_; primitives}

let read_type_parameter env (ctyp, var) =
  let open TypeDecl in
  let desc =
    match ctyp.ctyp_desc with
    | Ttyp_any -> Any
    | Ttyp_var s -> Var s
    | _ -> assert false
  in
  let var =
    match var with
    | Covariant -> Some Pos
    | Contravariant -> Some Neg
    | Invariant -> None
  in
    (desc, var)

let add_constructor_declaration container parent cd env =
  let env = add_attributes container cd.cd_attributes env in
  let env = Env.add_constructor parent cd.cd_id env in
    env

let read_constructor_declaration env container parent cd =
  let open TypeDecl.Constructor in
  let id = Identifier.Constructor(parent, Ident.name cd.cd_id) in
  let doc = read_attributes env container cd.cd_attributes in
  let args = List.map (read_core_type env) cd.cd_args in
  let res = opt_map (read_core_type env) cd.cd_res in
    {id; doc; args; res}

let add_label_declaration container parent ld env =
  let env = add_attributes container ld.ld_attributes env in
  let env = Env.add_field parent ld.ld_id env in
    env

let read_label_declaration env container parent ld =
  let open TypeDecl.Field in
  let id = Identifier.Field(parent, Ident.name ld.ld_id) in
  let doc = read_attributes env container ld.ld_attributes in
  let type_ = read_core_type env ld.ld_type in
    {id; doc; type_}

let add_type_kind container parent kind env =
  match kind with
  | Ttype_abstract -> env
  | Ttype_variant cstrs ->
      List.fold_right
        (add_constructor_declaration container parent)
        cstrs env
  | Ttype_record lbls ->
      List.fold_right
        (add_label_declaration container parent)
        lbls env
  | Ttype_open -> env

let read_type_kind env container parent =
  let open TypeDecl in function
    | Ttype_abstract -> None
    | Ttype_variant cstrs ->
        let cstrs =
          List.map (read_constructor_declaration env container parent) cstrs
        in
          Some (Variant cstrs)
    | Ttype_record lbls ->
        let lbls =
          List.map (read_label_declaration env container parent) lbls
        in
          Some (Record lbls)
    | Ttype_open -> Some Extensible

let add_type_declaration parent decl env =
  let container = Identifier.container_of_signature parent in
  let env = add_attributes container decl.typ_attributes env in
  let id = Identifier.Type(parent, Ident.name decl.typ_id) in
  let env = add_type_kind container id decl.typ_kind env in
  let env = Env.add_type parent decl.typ_id env in
    env

let read_type_declaration env parent decl =
  let open TypeDecl in
  let id = Identifier.Type(parent, Ident.name decl.typ_id) in
  let container = Identifier.container_of_signature parent in
  let doc = read_attributes env container decl.typ_attributes in
  let params = List.map (read_type_parameter env) decl.typ_params in
  let private_ = (decl.typ_private = Private) in
  let manifest = opt_map (read_core_type env) decl.typ_manifest in
  let constraints =
    List.map
      (fun (typ1, typ2, _) ->
         (read_core_type env typ1,
          read_core_type env typ2))
      decl.typ_cstrs
  in
  let kind = read_type_kind env container id decl.typ_kind in
    {id; doc; params; private_; manifest; constraints; kind}

let add_type_declarations parent decls env =
  let container = Identifier.container_of_signature parent in
  List.fold_right
    (fun decl env ->
       let env = add_comments container decl.typ_attributes env in
       let env = add_type_declaration parent decl env in
         env)
      decls env

let read_type_declarations env parent decls =
  let container = Identifier.container_of_signature parent in
  let items =
    List.fold_left
      (fun acc decl ->
         let open Signature in
         let comments = read_comments env container decl.typ_attributes in
         let comments = List.map (fun com -> Comment com) comments in
         let decl = read_type_declaration env parent decl in
           (Type decl) :: (List.rev_append comments acc))
      [] decls
  in
    List.rev items

let read_type_equation env decl =
  let open TypeDecl.Equation in
  let params = List.map (read_type_parameter env) decl.typ_params in
  let private_ = (decl.typ_private = Private) in
  let manifest =
    match decl.typ_manifest with
    | None -> assert false
    | Some typ -> read_core_type env typ
  in
    {params; private_; manifest}

let add_extension_constructor parent ext env =
  let container = Identifier.container_of_signature parent in
  let env = add_attributes container ext.ext_attributes env in
  let env = Env.add_extension parent ext.ext_id env in
    env

let read_extension_constructor env parent ext =
  let open Extension.Constructor in
  let id = Identifier.Extension(parent, Ident.name ext.ext_id) in
  let container = Identifier.container_of_signature parent in
  let doc = read_attributes env container ext.ext_attributes in
  match ext.ext_kind with
  | Text_rebind _ -> assert false
  | Text_decl(args, res) ->
      let args = List.map (read_core_type env) args in
      let res = opt_map (read_core_type env) res in
        {id; doc; args; res}

let add_type_extension parent tyext env =
  let container = Identifier.container_of_signature parent in
  let env = add_attributes container tyext.tyext_attributes env in
  let env =
    List.fold_right (add_extension_constructor parent)
      tyext.tyext_constructors env
  in
    env

let read_type_extension env parent tyext =
  let open Extension in
  let type_path = Env.Path.read_type env tyext.tyext_path in
  let container = Identifier.container_of_signature parent in
  let doc = read_attributes env container tyext.tyext_attributes in
  let type_params = List.map (read_type_parameter env) tyext.tyext_params in
  let private_ = (tyext.tyext_private = Private) in
  let constructors =
    List.map (read_extension_constructor env parent) tyext.tyext_constructors
  in
    { type_path; doc; type_params; private_; constructors; }

let add_exception parent ext env =
  let container = Identifier.container_of_signature parent in
  let env = add_attributes container ext.ext_attributes env in
  let env = Env.add_exception parent ext.ext_id env in
    env

let read_exception env parent ext =
  let open Exception in
  let id = Identifier.Exception(parent, Ident.name ext.ext_id) in
  let container = Identifier.container_of_signature parent in
  let doc = read_attributes env container ext.ext_attributes in
  match ext.ext_kind with
  | Text_rebind _ -> assert false
  | Text_decl(args, res) ->
      let args = List.map (read_core_type env) args in
      let res = opt_map (read_core_type env) res in
        {id; doc; args; res}

let add_class_type_field parent ctf env =
  let container = Identifier.container_of_class_signature parent in
  let env = add_attributes container ctf.ctf_attributes env in
  match ctf.ctf_desc with
  | Tctf_val(name, mutable_, virtual_, typ) ->
      let env = Env.add_instance_variable parent name env in
        env
  | Tctf_method(name, private_, virtual_, typ) ->
      let env = Env.add_method parent name env in
        env
  | Tctf_constraint _ -> env
  | Tctf_inherit cltyp ->
      let csig = Ctype.signature_of_class_type cltyp.cltyp_type in
      let env =
        Types.Vars.fold
          (fun name _ env -> Env.add_instance_variable parent name env)
          csig.csig_vars env
      in
      let methods, _ =
        Ctype.flatten_fields (Ctype.object_fields csig.csig_self)
      in
      let env =
        List.fold_right
          (fun (name, _, _) env -> Env.add_method parent name env)
          methods env
      in
        env
  | Tctf_attribute attr -> add_comment container attr env

let rec read_class_type_field env parent ctf =
  let open ClassSignature in
  let container = Identifier.container_of_class_signature parent in
  let doc = read_attributes env container ctf.ctf_attributes in
  match ctf.ctf_desc with
  | Tctf_val(name, mutable_, virtual_, typ) ->
      let open InstanceVariable in
      let id = Identifier.InstanceVariable(parent, name) in
      let mutable_ = (mutable_ = Mutable) in
      let virtual_ = (virtual_ = Virtual) in
      let type_ = read_core_type env typ in
        Some (InstanceVariable {id; doc; mutable_; virtual_; type_})
  | Tctf_method(name, private_, virtual_, typ) ->
      let open Method in
      let id = Identifier.Method(parent, name) in
      let private_ = (private_ = Private) in
      let virtual_ = (virtual_ = Virtual) in
      let type_ = read_core_type env typ in
        Some (Method {id; doc; private_; virtual_; type_})
  | Tctf_constraint(typ1, typ2) ->
      let typ1 = read_core_type env typ1 in
      let typ2 = read_core_type env typ2 in
        Some (Constraint(typ1, typ2))
  | Tctf_inherit cltyp ->
      Some (Inherit (read_class_signature env parent cltyp))
  | Tctf_attribute attr ->
      match read_comment env container attr with
      | None -> None
      | Some doc -> Some (Comment doc)

and read_class_signature env parent cltyp =
  let open ClassType in
    match cltyp.cltyp_desc with
    | Tcty_constr(p, _, params) ->
        let p = Env.Path.read_class_type env p in
        let params = List.map (read_core_type env) params in
          Constr(p, params)
    | Tcty_signature csig ->
        let open ClassSignature in
        let env =
          List.fold_right
            (add_class_type_field parent)
            csig.csig_fields env
        in
        let self =
          if csig.csig_self.ctyp_desc = Ttyp_any then None
          else Some (read_core_type env csig.csig_self)
        in
        let items =
          List.fold_left
            (fun rest item ->
               match read_class_type_field env parent item with
               | None -> rest
               | Some item -> item :: rest)
            [] csig.csig_fields
        in
        let items = List.rev items in
          Signature {self; items}
    | Tcty_arrow _ -> assert false

let add_class_type_declaration parent cltd env =
  let container = Identifier.container_of_signature parent in
  let env = add_attributes container cltd.ci_attributes env in
  let env = Env.add_class_type parent cltd.ci_id_class_type env in
    env

let read_class_type_declaration env parent cltd =
  let open ClassType in
  let name = Ident.name cltd.ci_id_class_type in
  let id = Identifier.ClassType(parent, name) in
  let container = Identifier.container_of_signature parent in
  let doc = read_attributes env container cltd.ci_attributes in
  let virtual_ = (cltd.ci_virt = Virtual) in
  let params = List.map (read_type_parameter env) cltd.ci_params in
  let expr = read_class_signature env id cltd.ci_expr in
    { id; doc; virtual_; params; expr }

let add_class_type_declarations parent cltds env =
  let container = Identifier.container_of_signature parent in
  List.fold_right
    (fun cltd env ->
       let env = add_comments container cltd.ci_attributes env in
       let env = add_class_type_declaration parent cltd env in
         env)
      cltds env

let read_class_type_declarations env parent cltds =
  let container = Identifier.container_of_signature parent in
  let items =
    List.fold_left
      (fun acc cltd ->
         let open Signature in
         let comments = read_comments env container cltd.ci_attributes in
         let comments = List.map (fun com -> Comment com) comments in
         let cltd = read_class_type_declaration env parent cltd in
           (ClassType cltd) :: (List.rev_append comments acc))
      [] cltds
  in
    List.rev items

let rec read_class_type env parent cty =
  let open Class in
  match cty.cltyp_desc with
  | Tcty_constr _ | Tcty_signature _ ->
      ClassType (read_class_signature env parent cty)
  | Tcty_arrow(lbl, arg, res) ->
      let lbl = read_label lbl in
      let arg = read_core_type env arg in
      let res = read_class_type env parent res in
        Arrow(lbl, arg, res)

let add_class_declaration parent cld env =
  let container = Identifier.container_of_signature parent in
  let env = add_attributes container cld.ci_attributes env in
  let env = Env.add_class parent cld.ci_id_class env in
    env

let read_class_declaration env parent cld =
  let open Class in
  let name = Ident.name cld.ci_id_class in
  let id = Identifier.Class(parent, name) in
  let container = Identifier.container_of_signature parent in
  let doc = read_attributes env container cld.ci_attributes in
  let virtual_ = (cld.ci_virt = Virtual) in
  let params = List.map (read_type_parameter env) cld.ci_params in
  let type_ = read_class_type env id cld.ci_expr in
    { id; doc; virtual_; params; type_ }

let add_class_declarations parent clds env =
  let container = Identifier.container_of_signature parent in
  List.fold_right
    (fun cld env ->
       let env = add_comments container cld.ci_attributes env in
       let env = add_class_declaration parent cld env in
         env)
      clds env

let read_class_declarations env parent clds =
  let container = Identifier.container_of_signature parent in
  let items =
    List.fold_left
      (fun acc cld ->
         let open Signature in
         let comments = read_comments env container cld.ci_attributes in
         let comments = List.map (fun com -> Comment com) comments in
         let cld = read_class_declaration env parent cld in
           (Class cld) :: (List.rev_append comments acc))
      [] clds
  in
    List.rev items

let add_module_type_declaration parent mtd env =
  let container = Identifier.container_of_signature parent in
  let env = add_attributes container mtd.mtd_attributes env in
  let env = Env.add_module_type parent mtd.mtd_id env in
    env

let add_module_declaration parent md env =
  let container = Identifier.container_of_signature parent in
  let env = add_attributes container md.md_attributes env in
  let env = Env.add_module parent md.md_id env in
    env

let add_module_declarations parent mds env =
  let container = Identifier.container_of_signature parent in
  List.fold_right
    (fun md env ->
       let env = add_comments container md.md_attributes env in
       let env = add_module_declaration parent md env in
         env)
      mds env

let add_signature_item parent item env =
  match item.sig_desc with
  | Tsig_value vd -> add_value_description parent vd env
  | Tsig_type decls -> add_type_declarations parent decls env
  | Tsig_typext tyext -> add_type_extension parent tyext env
  | Tsig_exception ext -> add_exception parent ext env
  | Tsig_module md -> add_module_declaration parent md env
  | Tsig_recmodule mds -> add_module_declarations parent mds env
  | Tsig_modtype mtd -> add_module_type_declaration parent mtd env
  | Tsig_open _ -> env
  | Tsig_include incl ->
      let items = incl.incl_type in
        List.fold_right
          (fun item env ->
             match item with
             | Sig_value(id, _) -> Env.add_value parent id env
             | Sig_type(id, _, _) -> Env.add_type parent id env
             | Sig_typext(id, _, (Text_first | Text_next)) -> Env.add_extension parent id env
             | Sig_typext(id, _, Text_exception) -> Env.add_exception parent id env
             | Sig_module(id, _, _) -> Env.add_module parent id env
             | Sig_modtype(id, _) -> Env.add_module_type parent id env
             | Sig_class(id, _, _) -> Env.add_class parent id env
             | Sig_class_type(id, _, _) -> Env.add_class_type parent id env)
          items env
  | Tsig_class cls -> add_class_declarations parent cls env
  | Tsig_class_type cltyps -> add_class_type_declarations parent cltyps env
  | Tsig_attribute attr ->
      add_comment (Identifier.container_of_signature parent) attr env

let rec read_with_constraint env parent (_, frag, constr) =
  let open ModuleType in
    match constr with
    | Twith_type decl ->
        let frag = Env.Fragment.read_type frag.Location.txt in
        let eq = read_type_equation env decl in
          TypeEq(frag, eq)
    | Twith_module(p, _) ->
        let frag = Env.Fragment.read_module frag.Location.txt in
        let eq = read_module_equation env p in
          ModuleEq(frag, eq)
    | Twith_typesubst decl ->
        let frag = Env.Fragment.read_type frag.Location.txt in
        let typ =
          match decl.typ_manifest with
          | None -> assert false
          | Some typ -> read_core_type env typ
        in
          TypeSubst(frag, typ)
    | Twith_modsubst(p, _) ->
        let frag = Env.Fragment.read_module frag.Location.txt in
        let p = Env.Path.read_module env p in
          ModuleSubst(frag, p)

and read_module_type env parent pos mty =
  let open ModuleType in
    match mty.mty_desc with
    | Tmty_ident(p, _) -> Ident (Env.Path.read_module_type env p)
    | Tmty_signature sg -> Signature (read_signature env parent sg)
    | Tmty_functor(id, _, arg, res) ->
        let arg =
          match arg with
          | None -> None
          | Some mty ->
              let name = Ident.name id in
              let id = Identifier.Argument(parent, pos, name) in
              let mty = read_module_type env id 1 mty in
                Some (name, mty)
        in
        let res = read_module_type env parent (pos + 1) mty in
          Functor(arg, res)
    | Tmty_with(body, subs) ->
        let body = read_module_type env parent pos body in
        let subs = List.map (read_with_constraint env parent) subs in
          With(body, subs)
    | Tmty_typeof mexpr ->
        let decl =
          let open Module in
          match mexpr.mod_desc with
          | Tmod_ident(p, _) -> Alias (Env.Path.read_module env p)
          | _ ->
              let mty =
                DocOckCmi.read_module_type env parent pos mexpr.mod_type
              in
                ModuleType mty
        in
          TypeOf decl
    | Tmty_alias _ -> assert false

and read_module_type_declaration env parent mtd =
  let open ModuleType in
  let name = Ident.name mtd.mtd_id in
  let id = Identifier.ModuleType(parent, name) in
  let container = Identifier.container_of_signature parent in
  let doc = read_attributes env container mtd.mtd_attributes in
  let expr = opt_map (read_module_type env id 1) mtd.mtd_type in
    {id; doc; expr}

and read_module_declaration env parent md =
  let open Module in
  let name = Ident.name md.md_id in
  let id = Identifier.Module(parent, name) in
  let container = Identifier.container_of_signature parent in
  let doc = read_attributes env container md.md_attributes in
  let type_ =
    match md.md_type.mty_desc with
    | Tmty_alias(p, _) -> Alias (Env.Path.read_module env p)
    | _ -> ModuleType (read_module_type env id 1 md.md_type)
  in
    {id; doc; type_}

and read_module_declarations env parent mds =
  let container = Identifier.container_of_signature parent in
  let items =
    List.fold_left
      (fun acc md ->
         let open Signature in
         let comments = read_comments env container md.md_attributes in
         let comments = List.map (fun com -> Comment com) comments in
         let md = read_module_declaration env parent md in
           (Module md) :: (List.rev_append comments acc))
      [] mds
  in
    List.rev items

and read_module_equation env p =
  let open Module in
    Alias (Env.Path.read_module env p)

and read_signature_item env parent item =
  let open Signature in
  match item.sig_desc with
  | Tsig_value vd ->
      [read_value_description env parent vd]
  | Tsig_type decls ->
      read_type_declarations env parent decls
  | Tsig_typext tyext ->
      [TypExt (read_type_extension env parent tyext)]
  | Tsig_exception ext ->
      [Exception (read_exception env parent ext)]
  | Tsig_module md ->
      [Module (read_module_declaration env parent md)]
  | Tsig_recmodule mds ->
      read_module_declarations env parent mds
  | Tsig_modtype mtd ->
      [ModuleType (read_module_type_declaration env parent mtd)]
  | Tsig_open _ -> []
  | Tsig_include incl ->
      let mty = read_module_type env parent 0 incl.incl_mod in
        [Include mty]
  | Tsig_class cls ->
      read_class_declarations env parent cls
  | Tsig_class_type cltyps ->
      read_class_type_declarations env parent cltyps
  | Tsig_attribute attr ->
      let container = Identifier.container_of_signature parent in
      match read_comment env container attr with
      | None -> []
      | Some doc -> [Comment doc]

and read_signature env parent sg =
  let env =
    List.fold_right
      (add_signature_item parent)
      sg.sig_items env
  in
  let items =
    List.fold_left
      (fun items item ->
         List.rev_append (read_signature_item env parent item) items)
      [] sg.sig_items
  in
    List.rev items

let read_interface root intf =
  let open Module in
  let id = Identifier.Root root in
  let items = read_signature Env.empty id intf in
  let doc, items =
    let open Signature in
    let open Documentation in
    match items with
    | Comment (Documentation doc) :: items -> doc, items
    | _ -> empty, items
  in
  let type_ = ModuleType (ModuleType.Signature items) in
    {id; doc; type_}
