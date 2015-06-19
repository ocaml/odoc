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

open DocOckPaths
open DocOckTypes
open DocOckPredef

open Identifier

module StringTbl = Map.Make(String)

type 'a type_ident =
  ('a, [`Type|`Class|`ClassType]) t

type 'a constructor_ident =
  ('a, [`Constructor|`Extension|`Exception]) t

type 'a extension_ident =
  ('a, [`Extension|`Exception]) t

type 'a class_type_ident =
  ('a, [`Class|`ClassType]) t

type 'a parent_ident =
  ('a, [`Module|`ModuleType|`Type|`Class|`ClassType]) t

let widen_module : 'a module_ -> _ = function
  | Root _ as id -> id
  | Module _ as id -> id
  | Argument _ as id -> id

let widen_module_type : 'a module_type -> _ = function
  | ModuleType _ as id -> id

let widen_type : 'a type_ -> _ = function
  | Type _ as id -> id
  | CoreType _ as id -> id

let widen_constructor : 'a constructor -> _ = function
  | Constructor _ as id -> id

let widen_field : 'a field -> _ = function
  | Field _ as id -> id

let widen_extension : 'a extension -> _ = function
  | Extension _ as id -> id

let widen_exception : 'a exception_ -> _ = function
  | Exception _ as id -> id
  | CoreException _ as id -> id

let widen_value : 'a value -> _ = function
  | Value _ as id -> id

let widen_class : 'a class_ -> _ = function
  | Class _ as id -> id

let widen_class_type : 'a class_type -> _ = function
  | ClassType _ as id -> id

let widen_method : 'a method_ -> _ = function
  | Method _ as id -> id

let widen_instance_variable : 'a instance_variable -> _ = function
  | InstanceVariable _ as id -> id

let widen_label : 'a label -> _ = function
  | Label _ as id -> id

type 'a t =
  { modules : 'a module_ StringTbl.t;
    module_types : 'a module_type StringTbl.t;
    types : 'a type_ident StringTbl.t;
    constructors : 'a constructor_ident StringTbl.t;
    fields : 'a field StringTbl.t;
    extensions : 'a extension_ident StringTbl.t;
    exceptions : 'a exception_ StringTbl.t;
    values : 'a value StringTbl.t;
    classes : 'a class_ StringTbl.t;
    class_types : 'a class_type_ident StringTbl.t;
    methods : 'a method_ StringTbl.t;
    instance_variables : 'a instance_variable StringTbl.t;
    labels : 'a label StringTbl.t;
    parents : 'a parent_ident StringTbl.t;
    elements : 'a any StringTbl.t; }

let empty =
  { modules = StringTbl.empty;
    module_types = StringTbl.empty;
    types = StringTbl.empty;
    constructors = StringTbl.empty;
    fields = StringTbl.empty;
    extensions = StringTbl.empty;
    exceptions = StringTbl.empty;
    values = StringTbl.empty;
    classes = StringTbl.empty;
    class_types = StringTbl.empty;
    methods = StringTbl.empty;
    instance_variables = StringTbl.empty;
    labels = StringTbl.empty;
    parents = StringTbl.empty;
    elements = StringTbl.empty; }

let opt_fold f o acc =
  match o with
  | None -> acc
  | Some x -> f x acc

let add_label id env =
  let name = Identifier.name id in
  let elements =
    StringTbl.add name (widen_label id) env.elements
  in
  let labels =
    StringTbl.add name id env.labels
  in
    { env with elements; labels }

let rec add_text_element elem env =
  let open Documentation in
    match elem with
    | Raw _ | Code _ | PreCode _ | Verbatim _
    | Newline | Special _ | Target _ | Reference(_, None) -> env
    | Style(_, txt) | Reference(_, Some txt) -> add_text txt env
    | List l -> List.fold_right add_text l env
    | Enum l -> List.fold_right add_text l env
    | Title(_, l, txt) ->
        let env = add_text txt env in
        let env =
          match l with
          | None -> env
          | Some id -> add_label id env
        in
          env

and add_text txt env =
  List.fold_right add_text_element txt env

let add_tag tag env =
  let open Documentation in
    match tag with
    | Author _ | Version _ | Since _ -> env
    | See(_, txt) | Before(_, txt) | Deprecated txt
    | Param(_, txt) | Raise(_, txt)
    | Return txt | Tag(_, txt) -> add_text txt env

let add_documentation doc env =
  let open Documentation in
  match doc with
  | Ok {text; tags} ->
      let env = add_text text env in
      let env = List.fold_right add_tag tags env in
        env
  | Error _ -> env

let add_comment com env =
  let open Documentation in
  match com with
  | Documentation doc -> add_documentation doc env
  | Stop -> env

let add_value vl env =
  let open Value in
  let env = add_documentation vl.doc env in
  let name = Identifier.name vl.id in
  let elements =
    StringTbl.add name (widen_value vl.id) env.elements
  in
  let values =
    StringTbl.add name vl.id env.values
  in
    { env with elements; values }

let add_external xt env =
  let open External in
  let env = add_documentation xt.doc env in
  let name = Identifier.name xt.id in
  let elements =
    StringTbl.add name (widen_value xt.id) env.elements
  in
  let values =
    StringTbl.add name xt.id env.values
  in
    { env with elements; values }

let add_constructor cstr env =
  let open TypeDecl.Constructor in
  let env = add_documentation cstr.doc env in
  let name = Identifier.name cstr.id in
  let elements =
    StringTbl.add name (widen_constructor cstr.id) env.elements
  in
  let constructors =
    StringTbl.add name (widen_constructor cstr.id) env.constructors
  in
    { env with elements; constructors }

let add_field field env =
  let open TypeDecl.Field in
  let env = add_documentation field.doc env in
  let name = Identifier.name field.id in
  let elements =
    StringTbl.add name (widen_field field.id) env.elements
  in
  let fields =
    StringTbl.add name field.id env.fields
  in
    { env with elements; fields }

let add_representation repr env =
  let open TypeDecl.Representation in
  match repr with
  | Variant cstrs -> List.fold_right add_constructor cstrs env
  | Record fields -> List.fold_right add_field fields env
  | Extensible -> env

let add_type_decl decl env =
  let open TypeDecl in
  let env  = add_documentation decl.doc env in
  let env = opt_fold add_representation decl.representation env in
  let name = Identifier.name decl.id in
  let elements =
    StringTbl.add name (widen_type decl.id) env.elements
  in
  let parents =
    StringTbl.add name (widen_type decl.id) env.parents
  in
  let types =
    StringTbl.add name (widen_type decl.id) env.types
  in
    { env with elements; parents; types }

let add_extension_constructor ext env =
  let open Extension.Constructor in
  let env = add_documentation ext.doc env in
  let name = Identifier.name ext.id in
  let elements =
    StringTbl.add name (widen_extension ext.id) env.elements
  in
  let constructors =
    StringTbl.add name (widen_extension ext.id) env.constructors
  in
  let extensions =
    StringTbl.add name (widen_extension ext.id) env.extensions
  in
    { env with elements; constructors; extensions }

let add_extension tyext env =
  let open Extension in
  let env = add_documentation tyext.doc env in
  let env =
    List.fold_right add_extension_constructor
      tyext.constructors env
  in
    env

let add_exception exn env =
  let open Exception in
  let env = add_documentation exn.doc env in
  let name = Identifier.name exn.id in
  let elements =
    StringTbl.add name (widen_exception exn.id) env.elements
  in
  let constructors =
    StringTbl.add name (widen_exception exn.id) env.constructors
  in
  let extensions =
    StringTbl.add name (widen_exception exn.id) env.extensions
  in
  let exceptions =
    StringTbl.add name exn.id env.exceptions
  in
    { env with elements; constructors; extensions; exceptions }

let add_class_type cltyp env =
  let open ClassType in
  let env = add_documentation cltyp.doc env in
  let name = Identifier.name cltyp.id in
  let elements =
    StringTbl.add name (widen_class_type cltyp.id) env.elements
  in
  let parents =
    StringTbl.add name (widen_class_type cltyp.id) env.parents
  in
  let types =
    StringTbl.add name (widen_class_type cltyp.id) env.types
  in
  let class_types =
    StringTbl.add name (widen_class_type cltyp.id) env.class_types
  in
    { env with elements; parents; types; class_types }

let add_class cl env =
  let open Class in
  let env = add_documentation cl.doc env in
  let name = Identifier.name cl.id in
  let elements =
    StringTbl.add name (widen_class cl.id) env.elements
  in
  let parents =
    StringTbl.add name (widen_class cl.id) env.parents
  in
  let types =
    StringTbl.add name (widen_class cl.id) env.types
  in
  let class_types =
    StringTbl.add name (widen_class cl.id) env.class_types
  in
  let classes =
    StringTbl.add name (widen_class cl.id) env.classes
  in
    { env with elements; parents; types; class_types; classes }

let add_module_type mtyp env =
  let open ModuleType in
  let env = add_documentation mtyp.doc env in
  let name = Identifier.name mtyp.id in
  let elements =
    StringTbl.add name (widen_module_type mtyp.id) env.elements
  in
  let parents =
    StringTbl.add name (widen_module_type mtyp.id) env.parents
  in
  let module_types =
    StringTbl.add name (widen_module_type mtyp.id) env.module_types
  in
    { env with elements; parents; module_types }

let add_module md env =
  let open Module in
  let env = add_documentation md.doc env in
  let name = Identifier.name md.id in
  let elements =
    StringTbl.add name (widen_module md.id) env.elements
  in
  let parents =
    StringTbl.add name (widen_module md.id) env.parents
  in
  let modules =
    StringTbl.add name (widen_module md.id) env.modules
  in
    { env with elements; parents; modules }

let add_signature_item item env =
  let open Signature in
  match item with
  | Module md -> add_module md env
  | ModuleType mtyp -> add_module_type mtyp env
  | Type decl -> add_type_decl decl env
  | TypExt tyext -> add_extension tyext env
  | Exception exn -> add_exception exn env
  | Value vl -> add_value vl env
  | External ext -> add_external ext env
  | Class cl -> add_class cl env
  | ClassType cltyp -> add_class_type cltyp env
  | Include _ -> env
  | Comment com -> add_comment com env

let add_signature_items sg env =
  List.fold_right add_signature_item sg env

let add_method meth env =
  let open Method in
  let env = add_documentation meth.doc env in
  let name = Identifier.name meth.id in
  let elements =
    StringTbl.add name (widen_method meth.id) env.elements
  in
  let methods =
    StringTbl.add name (widen_method meth.id) env.methods
  in
    { env with elements; methods }

let add_instance_variable inst env =
  let open InstanceVariable in
  let env = add_documentation inst.doc env in
  let name = Identifier.name inst.id in
  let elements =
    StringTbl.add name (widen_instance_variable inst.id) env.elements
  in
  let instance_variables =
    StringTbl.add name (widen_instance_variable inst.id)
      env.instance_variables
  in
    { env with elements; instance_variables }

let add_class_signature_item item env =
  let open ClassSignature in
  match item with
  | Method meth -> add_method meth env
  | InstanceVariable inst -> add_instance_variable inst env
  | Constraint _ -> env
  | Inherit _ -> env
  | Comment com -> add_comment com env

let add_class_signature_items clsig env =
  let open ClassSignature in
    List.fold_right
      add_class_signature_item
      clsig.items env

open DocOckPaths.Reference.Resolved
open DocOckPaths.Reference

let lookup_module_ident env name =
  try
    let id = StringTbl.find name env.modules in
      Resolved (Identifier id)
  with Not_found -> Root name

let lookup_module_type_ident env name =
  try
    let id = StringTbl.find name env.module_types in
      Resolved (Identifier id)
  with Not_found -> Root name

let lookup_type_ident env name =
  try
    let id = StringTbl.find name env.types in
      Resolved (Identifier id)
  with Not_found ->
    match core_type_identifier name with
    | Some id -> Resolved (Identifier id)
    | None -> Root name

let lookup_constructor_ident env name =
  try
    let id = StringTbl.find name env.constructors in
      Resolved (Identifier id)
  with Not_found ->
    match core_constructor_identifier name with
    | Some id -> Resolved (Identifier id)
    | None ->
        match core_exception_identifier name with
        | Some id -> Resolved (Identifier id)
        | None -> Root name

let lookup_field_ident env name =
  try
    let id = StringTbl.find name env.fields in
      Resolved (Identifier id)
  with Not_found -> Root name

let lookup_extension_ident env name =
  try
    let id = StringTbl.find name env.extensions in
      Resolved (Identifier id)
  with Not_found ->
    match core_exception_identifier name with
    | Some id -> Resolved (Identifier id)
    | None -> Root name

let lookup_exception_ident env name =
  try
    let id = StringTbl.find name env.exceptions in
      Resolved (Identifier id)
  with Not_found ->
    match core_exception_identifier name with
    | Some id -> Resolved (Identifier id)
    | None -> Root name

let lookup_value_ident env name =
  try
    let id = StringTbl.find name env.values in
      Resolved (Identifier id)
  with Not_found -> Root name

let lookup_class_ident env name =
  try
    let id = StringTbl.find name env.classes in
      Resolved (Identifier id)
  with Not_found -> Root name

let lookup_class_type_ident env name =
  try
    let id = StringTbl.find name env.class_types in
      Resolved (Identifier id)
  with Not_found -> Root name

let lookup_method_ident env name =
  try
    let id = StringTbl.find name env.methods in
    Resolved (Identifier id)
  with Not_found -> Root name

let lookup_instance_variable_ident env name =
  try
    let id = StringTbl.find name env.instance_variables in
      Resolved (Identifier id)
  with Not_found -> Root name

let lookup_label_ident env name =
  try
    let id = StringTbl.find name env.labels in
      Resolved (Identifier id)
  with Not_found -> Root name

let lookup_parent_ident env name =
  try
    let id = StringTbl.find name env.parents in
      Resolved (Identifier id)
  with Not_found ->
    match core_type_identifier name with
    | Some id -> Resolved (Identifier id)
    | None -> Root name

let lookup_element_ident env name =
  try
    let id = StringTbl.find name env.elements in
      Resolved (Identifier id)
  with Not_found ->
    match core_type_identifier name with
    | Some id -> Resolved (Identifier id)
    | None ->
        match core_constructor_identifier name with
        | Some id -> Resolved (Identifier id)
        | None ->
            match core_exception_identifier name with
            | Some id -> Resolved (Identifier id)
            | None -> Root name

let rec lookup_parent env = function
  | Resolved _ as r -> r
  | Root s -> lookup_parent_ident env s
  | Dot(r, s) -> Dot(lookup_parent env r, s)

let lookup_module env = function
  | Resolved _ as r -> r
  | Root s -> lookup_module_ident env s
  | Dot(r, s) -> Dot(lookup_parent env r, s)

let lookup_module_type env = function
  | Resolved _ as r -> r
  | Root s -> lookup_module_type_ident env s
  | Dot(r, s) -> Dot(lookup_parent env r, s)

let lookup_type env = function
  | Resolved _ as r -> r
  | Root s -> lookup_type_ident env s
  | Dot(r, s) -> Dot(lookup_parent env r, s)

let lookup_constructor env = function
  | Resolved _ as r -> r
  | Root s -> lookup_constructor_ident env s
  | Dot(r, s) -> Dot(lookup_parent env r, s)

let lookup_field env = function
  | Resolved _ as r -> r
  | Root s -> lookup_field_ident env s
  | Dot(r, s) -> Dot(lookup_parent env r, s)

let lookup_extension env = function
  | Resolved _ as r -> r
  | Root s -> lookup_extension_ident env s
  | Dot(r, s) -> Dot(lookup_parent env r, s)

let lookup_exception env = function
  | Resolved _ as r -> r
  | Root s -> lookup_exception_ident env s
  | Dot(r, s) -> Dot(lookup_parent env r, s)

let lookup_value env = function
  | Resolved _ as r -> r
  | Root s -> lookup_value_ident env s
  | Dot(r, s) -> Dot(lookup_parent env r, s)

let lookup_class env = function
  | Resolved _ as r -> r
  | Root s -> lookup_class_ident env s
  | Dot(r, s) -> Dot(lookup_parent env r, s)

let lookup_class_type env = function
  | Resolved _ as r -> r
  | Root s -> lookup_class_type_ident env s
  | Dot(r, s) -> Dot(lookup_parent env r, s)

let lookup_method env = function
  | Resolved _ as r -> r
  | Root s -> lookup_method_ident env s
  | Dot(r, s) -> Dot(lookup_parent env r, s)

let lookup_instance_variable env = function
  | Resolved _ as r -> r
  | Root s -> lookup_instance_variable_ident env s
  | Dot(r, s) -> Dot(lookup_parent env r, s)

let lookup_label env = function
  | Resolved _ as r -> r
  | Root s -> lookup_label_ident env s
  | Dot(r, s) -> Dot(lookup_parent env r, s)

let lookup_element env = function
  | Resolved _ as r -> r
  | Root s -> lookup_element_ident env s
  | Dot(r, s) -> Dot(lookup_parent env r, s)
