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

open Model.Paths
open Model.Lang
open Model.Predefined

open Identifier

module StringTbl = Map.Make(String)

type type_ident =
  [`Type | `Class | `ClassType] t

type constructor_ident =
  [`Constructor|`Extension|`Exception] t

type extension_ident =
  [`Extension|`Exception] t

type class_type_ident =
  [`Class|`ClassType] t

type parent_ident =
  [`Module|`ModuleType|`Type|`Class|`ClassType|`Page] t

type signature_ident =
  [`Module|`ModuleType] t

let widen_module : module_ -> _ = function
  | Root _ as id -> id
  | Module _ as id -> id
  | Argument _ as id -> id

let widen_module_type : module_type -> _ = function
  | ModuleType _ as id -> id

let widen_type : type_ -> _ = function
  | Type _ as id -> id
  | CoreType _ as id -> id

let widen_constructor : constructor -> _ = function
  | Constructor _ as id -> id

let widen_field : field -> _ = function
  | Field _ as id -> id

let widen_extension : extension -> _ = function
  | Extension _ as id -> id

let widen_exception : exception_ -> _ = function
  | Exception _ as id -> id
  | CoreException _ as id -> id

let widen_value : value -> _ = function
  | Value _ as id -> id

let widen_class : class_ -> _ = function
  | Class _ as id -> id

let widen_class_type : class_type -> _ = function
  | ClassType _ as id -> id

let widen_method : method_ -> _ = function
  | Method _ as id -> id

let widen_instance_variable : instance_variable -> _ = function
  | InstanceVariable _ as id -> id

let widen_label : label -> _ = function
  | Label _ as id -> id

let widen_page : page -> _ = function
  | Page _ as id -> id

type t =
  { modules : module_ StringTbl.t;
    module_types : module_type StringTbl.t;
    types : type_ident StringTbl.t;
    constructors : constructor_ident StringTbl.t;
    fields : field StringTbl.t;
    extensions : extension_ident StringTbl.t;
    exceptions : exception_ StringTbl.t;
    values : value StringTbl.t;
    classes : class_ StringTbl.t;
    class_types : class_type_ident StringTbl.t;
    methods : method_ StringTbl.t;
    instance_variables : instance_variable StringTbl.t;
    labels : label StringTbl.t;
    parents : parent_ident StringTbl.t;
    elements : any StringTbl.t;
    titles : Model.Comment.link_content StringTbl.t; (* Hack *)
    signatures : signature_ident StringTbl.t;
  }

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
    elements = StringTbl.empty;
    titles = StringTbl.empty;
    signatures = StringTbl.empty
  }

let add_label_ident id env =
  let name = Identifier.name id in
  let elements =
    StringTbl.add name (widen_label id) env.elements
  in
  let labels =
    StringTbl.add name id env.labels
  in
    { env with elements; labels }

let add_value_ident id env =
  let name = Identifier.name id in
  let elements =
    StringTbl.add name (widen_value id) env.elements
  in
  let values =
    StringTbl.add name id env.values
  in
    { env with elements; values }

let add_external_ident id env =
  let name = Identifier.name id in
  let elements =
    StringTbl.add name (widen_value id) env.elements
  in
  let values =
    StringTbl.add name id env.values
  in
    { env with elements; values }

let add_constructor_ident id env =
  let name = Identifier.name id in
  let elements =
    StringTbl.add name (widen_constructor id) env.elements
  in
  let constructors =
    StringTbl.add name (widen_constructor id) env.constructors
  in
    { env with elements; constructors }

let add_field_ident id env =
  let name = Identifier.name id in
  let elements =
    StringTbl.add name (widen_field id) env.elements
  in
  let fields =
    StringTbl.add name id env.fields
  in
    { env with elements; fields }

let add_type_ident id env =
  let name = Identifier.name id in
  let elements =
    StringTbl.add name (widen_type id) env.elements
  in
  let parents =
    StringTbl.add name (widen_type id) env.parents
  in
  let types =
    StringTbl.add name (widen_type id) env.types
  in
    { env with elements; parents; types }

let add_extension_constructor_ident id env =
  let name = Identifier.name id in
  let elements =
    StringTbl.add name (widen_extension id) env.elements
  in
  let constructors =
    StringTbl.add name (widen_extension id) env.constructors
  in
  let extensions =
    StringTbl.add name (widen_extension id) env.extensions
  in
    { env with elements; constructors; extensions }

let add_exception_ident id env =
  let name = Identifier.name id in
  let elements =
    StringTbl.add name (widen_exception id) env.elements
  in
  let constructors =
    StringTbl.add name (widen_exception id) env.constructors
  in
  let extensions =
    StringTbl.add name (widen_exception id) env.extensions
  in
  let exceptions =
    StringTbl.add name id env.exceptions
  in
    { env with elements; constructors; extensions; exceptions }

let add_class_type_ident id env =
  let name = Identifier.name id in
  let elements =
    StringTbl.add name (widen_class_type id) env.elements
  in
  let parents =
    StringTbl.add name (widen_class_type id) env.parents
  in
  let types =
    StringTbl.add name (widen_class_type id) env.types
  in
  let class_types =
    StringTbl.add name (widen_class_type id) env.class_types
  in
    { env with elements; parents; types; class_types }

let add_class_ident id env =
  let name = Identifier.name id in
  let elements =
    StringTbl.add name (widen_class id) env.elements
  in
  let parents =
    StringTbl.add name (widen_class id) env.parents
  in
  let types =
    StringTbl.add name (widen_class id) env.types
  in
  let class_types =
    StringTbl.add name (widen_class id) env.class_types
  in
  let classes =
    StringTbl.add name (widen_class id) env.classes
  in
    { env with elements; parents; types; class_types; classes }

let add_module_type_ident id env =
  let name = Identifier.name id in
  let elements =
    StringTbl.add name (widen_module_type id) env.elements
  in
  let parents =
    StringTbl.add name (widen_module_type id) env.parents
  in
  let module_types =
    StringTbl.add name (widen_module_type id) env.module_types
  in
  let signatures =
    StringTbl.add name (widen_module_type id) env.signatures
  in
    { env with elements; parents; module_types; signatures }

let add_module_ident id env =
  let name = Identifier.name id in
  let elements =
    StringTbl.add name (widen_module id) env.elements
  in
  let parents =
    StringTbl.add name (widen_module id) env.parents
  in
  let modules =
    StringTbl.add name (widen_module id) env.modules
  in
  let signatures =
    StringTbl.add name (widen_module id) env.signatures
  in
    { env with elements; parents; modules; signatures }

let add_page_ident id env =
  let name = Identifier.name id in
  let parents = StringTbl.add name (widen_page id) env.parents in
    { env with parents }

let opt_fold f o acc =
  match o with
  | None -> acc
  | Some x -> f x acc

let add_label_ident_title id txt env =
  let name = Identifier.name id in
  let titles =
    StringTbl.add name txt env.titles
  in
    { env with titles }

let add_documentation doc env =
  List.fold_right (fun element env ->
    match element.Model.Location_.value with
    | `Heading (_, label, nested_elements) ->
      let env = add_label_ident label env in
      let env = add_label_ident_title label nested_elements env in
      env
    | _ -> env)
    doc env

let add_comment com env =
  match com with
  | `Docs doc -> add_documentation doc env
  | `Stop -> env

let add_value vl env =
  let open Value in
  let env = add_documentation vl.doc env in
    add_value_ident vl.id env

let add_external xt env =
  let open External in
  let env = add_documentation xt.doc env in
    add_external_ident xt.id env

let add_constructor cstr env =
  let open TypeDecl.Constructor in
  let env = add_documentation cstr.doc env in
    add_constructor_ident cstr.id env

let add_field field env =
  let open TypeDecl.Field in
  let env = add_documentation field.doc env in
    add_field_ident field.id env

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
    add_type_ident decl.id env

let add_extension_constructor ext env =
  let open Extension.Constructor in
  let env = add_documentation ext.doc env in
    add_extension_constructor_ident ext.id env

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
    add_exception_ident exn.id env

let add_class_type cltyp env =
  let open ClassType in
  let env = add_documentation cltyp.doc env in
    add_class_type_ident cltyp.id env

let add_class cl env =
  let open Class in
  let env = add_documentation cl.doc env in
    add_class_ident cl.id env

let add_module_type mtyp env =
  let open ModuleType in
  let env = add_documentation mtyp.doc env in
    add_module_type_ident mtyp.id env

let add_module md env =
  let open Module in
  let env = add_documentation md.doc env in
    add_module_ident md.id env

let add_unit unit env =
  let open Compilation_unit in
  let env = add_documentation unit.doc env in
    add_module_ident unit.id env

let add_page page env =
  let open Page in
  let env = add_documentation page.content env in
    add_page_ident page.name env

let rec add_include incl env =
  let open Include in
  let env = add_documentation incl.doc env in
  add_signature_items incl.expansion.content env

and add_signature_item item env =
  let open Signature in
  match item with
  | Module (_, md) -> add_module md env
  | ModuleType mtyp -> add_module_type mtyp env
  | Type (_, decl) -> add_type_decl decl env
  | TypExt tyext -> add_extension tyext env
  | Exception exn -> add_exception exn env
  | Value vl -> add_value vl env
  | External ext -> add_external ext env
  | Class (_, cl) -> add_class cl env
  | ClassType (_, cltyp) -> add_class_type cltyp env
  | Include incl -> add_include incl env
  | Comment com -> add_comment com env

and add_signature_items sg env =
  List.fold_right add_signature_item sg env

let rec add_module_type_expr_items expr env =
  let open ModuleType in
    match expr with
    | Path _ -> env
    | Signature sg -> add_signature_items sg env
    | Functor(None, expr) -> add_module_type_expr_items expr env
    | Functor(Some{ FunctorArgument. id; _ }, expr) ->
      add_module_ident id
        (add_module_type_expr_items expr env)
    | With(expr, _) -> add_module_type_expr_items expr env
    | TypeOf decl -> add_module_decl_items decl env

and add_module_decl_items decl env =
  let open Module in
    match decl with
    | Alias _ -> env
    | ModuleType expr -> add_module_type_expr_items expr env

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

let add_class_type_expr_items expr env =
  let open ClassType in
    match expr with
    | Constr _ -> env
    | Signature clsig -> add_class_signature_items clsig env

let rec add_class_decl_items decl env =
  let open Class in
    match decl with
    | ClassType expr -> add_class_type_expr_items expr env
    | Arrow(_, _, decl) -> add_class_decl_items decl env

open Model.Paths.Reference.Resolved
open Model.Paths.Reference

let lookup_signature_ident env name =
  try
    let id = StringTbl.find name env.signatures in
      Resolved (Identifier id)
  with Not_found -> Root (name, TUnknown)

let lookup_module_ident env name =
  try
    let id = StringTbl.find name env.modules in
      Resolved (Identifier id)
  with Not_found -> Root (name, TModule)

let lookup_module_type_ident env name =
  try
    let id = StringTbl.find name env.module_types in
      Resolved (Identifier id)
  with Not_found -> Root (name, TModuleType)

let lookup_type_ident env name =
  try
    let id = StringTbl.find name env.types in
      Resolved (Identifier id)
  with Not_found ->
    match core_type_identifier name with
    | Some id -> Resolved (Identifier id)
    | None -> Root (name, TType)

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
        | None -> Root (name, TConstructor)

let lookup_field_ident env name =
  try
    let id = StringTbl.find name env.fields in
      Resolved (Identifier id)
  with Not_found -> Root (name, TField)

let lookup_extension_ident env name =
  try
    let id = StringTbl.find name env.extensions in
      Resolved (Identifier id)
  with Not_found ->
    match core_exception_identifier name with
    | Some id -> Resolved (Identifier id)
    | None -> Root (name, TExtension)

let lookup_exception_ident env name =
  try
    let id = StringTbl.find name env.exceptions in
      Resolved (Identifier id)
  with Not_found ->
    match core_exception_identifier name with
    | Some id -> Resolved (Identifier id)
    | None -> Root (name, TException)

let lookup_value_ident env name =
  try
    let id = StringTbl.find name env.values in
      Resolved (Identifier id)
  with Not_found -> Root (name, TValue)

let lookup_class_ident env name =
  try
    let id = StringTbl.find name env.classes in
      Resolved (Identifier id)
  with Not_found -> Root (name, TClass)

let lookup_class_type_ident env name =
  try
    let id = StringTbl.find name env.class_types in
      Resolved (Identifier id)
  with Not_found -> Root (name, TClassType)

let lookup_method_ident env name =
  try
    let id = StringTbl.find name env.methods in
    Resolved (Identifier id)
  with Not_found -> Root (name, TMethod)

let lookup_instance_variable_ident env name =
  try
    let id = StringTbl.find name env.instance_variables in
      Resolved (Identifier id)
  with Not_found -> Root (name, TInstanceVariable)

let lookup_label_ident env name =
  try
    let id = StringTbl.find name env.labels in
      Resolved (Identifier id)
  with Not_found -> Root (name, TLabel)

let lookup_parent_ident env name : Reference.parent =
  match StringTbl.find name env.parents with
  | Page _ ->
    assert false
  | Root _ | Module _ | Argument _ | ModuleType _ | Type _ | CoreType _
  | Class _ | ClassType _ as id ->
    Resolved (Identifier id)
  | exception Not_found ->
    match core_type_identifier name with
    | Some id -> Resolved (Identifier id)
    | None -> Root (name, TUnknown)

let lookup_label_parent_ident env name : Reference.label_parent =
  try
    let id = StringTbl.find name env.parents in
      Resolved (Identifier id)
  with Not_found ->
    match core_type_identifier name with
    | Some id -> Resolved (Identifier id)
    | None -> Root (name, TUnknown)

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
            | None -> Root (name, TUnknown)

let rec lookup_parent env : Reference.parent -> Reference.parent =
  function
  | Resolved _ as r -> r
  | Root (s, TUnknown) -> lookup_parent_ident env s
  | Root (s, TModule) ->
    lookup_module_ident env s
    |> signature_of_module
    |> parent_of_signature
  | Root (s,TModuleType) ->
    lookup_module_type_ident env s
    |> signature_of_module_type
    |> parent_of_signature
  | Root (s,TType) as r ->
    begin match lookup_type_ident env s with
    | Type _ | Class _ | ClassType _ | Dot _ ->
      (* can't go from Root to any of these. *)
      assert false
    | Root _ -> r
    | Resolved (Identifier (CoreType _ | Type _) | Type _) as resolved ->
      parent_of_datatype resolved
    | Resolved (Identifier (Class _ | ClassType _) | Class _ | ClassType _) as r
      -> parent_of_class_signature r
    end
  | Root (s,TClass) ->
    lookup_class_ident env s
    |> class_signature_of_class
    |> parent_of_class_signature
  | Root (s,TClassType) ->
    lookup_class_type_ident env s
    |> class_signature_of_class_type
    |> parent_of_class_signature
  | Dot(r, s) -> Dot(lookup_label_parent env r, s)
  | Module(r, s) -> Module(lookup_signature env r, s)
  | ModuleType(r, s) -> ModuleType(lookup_signature env r, s)
  | Type(r, s) -> Type(lookup_signature env r, s)
  | Class(r, s) -> Class(lookup_signature env r, s)
  | ClassType(r, s) -> ClassType(lookup_signature env r, s)

and lookup_label_parent env :
  Reference.label_parent -> Reference.label_parent =
  function
  | Resolved _ as r -> r
  | Root (s, TUnknown) -> lookup_label_parent_ident env s
  | Root (_, TPage) as r -> r (* there are no local pages. *)
  | Root (s, TModule) ->
    lookup_module_ident env s
    |> signature_of_module
    |> parent_of_signature
    |> label_parent_of_parent
  | Root (s,TModuleType) ->
    lookup_module_type_ident env s
    |> signature_of_module_type
    |> parent_of_signature
    |> label_parent_of_parent
  | Root (s,TType) as r ->
    begin match lookup_type_ident env s with
    | Type _ | Class _ | ClassType _ | Dot _ ->
      (* can't go from Root to any of these. *)
      assert false
    | Root _ -> r
    | Resolved (Identifier (CoreType _ | Type _) | Type _) as resolved ->
      parent_of_datatype resolved
    | Resolved (Identifier (Class _ | ClassType _) | Class _ | ClassType _) as r
      -> parent_of_class_signature r
    end
    |> label_parent_of_parent
  | Root (s,TClass) ->
    lookup_class_ident env s
    |> class_signature_of_class
    |> parent_of_class_signature
    |> label_parent_of_parent
  | Root (s,TClassType) ->
    lookup_class_type_ident env s
    |> class_signature_of_class_type
    |> parent_of_class_signature
    |> label_parent_of_parent
  | Dot(r, s) -> Dot(lookup_label_parent env r, s)
  | Module(r, s) -> Module(lookup_signature env r, s)
  | ModuleType(r, s) -> ModuleType(lookup_signature env r, s)
  | Type(r, s) -> Type(lookup_signature env r, s)
  | Class(r, s) -> Class(lookup_signature env r, s)
  | ClassType(r, s) -> ClassType(lookup_signature env r, s)

and lookup_signature env :
  Reference.signature -> Reference.signature = function
  | Resolved _ as r -> r
  | Root (s, TUnknown)    -> lookup_signature_ident env s
  | Root (s, TModule)     -> signature_of_module (lookup_module_ident env s)
  | Root (s, TModuleType) ->
    signature_of_module_type (lookup_module_type_ident env s)
  | Dot (p, s) -> Dot (lookup_label_parent env p, s)
  | Module (p,s) -> Module (lookup_signature env p, s)
  | ModuleType (p,s) -> ModuleType(lookup_signature env p, s)

let lookup_module env = function
  | Resolved _ as r -> r
  | Root (s, _) -> lookup_module_ident env s
  | Dot(r, s) -> Dot(lookup_label_parent env r, s)
  | Module(p, s) -> Module(lookup_signature env p, s)

let lookup_module_type env = function
  | Resolved _ as r -> r
  | Root (s, _) -> lookup_module_type_ident env s
  | Dot(r, s) -> Dot(lookup_label_parent env r, s)
  | ModuleType(p, s) -> ModuleType(lookup_signature env p, s)

let lookup_type env = function
  | Resolved _ as r -> r
  | Root (s, _) -> lookup_type_ident env s
  | Dot(r, s) -> Dot(lookup_label_parent env r, s)
  | Type(r, s) -> Type(lookup_signature env r, s)
  | Class(r, s) -> Class(lookup_signature env r, s)
  | ClassType(r, s) -> ClassType(lookup_signature env r, s)

let lookup_datatype env : Reference.datatype -> Reference.datatype = function
  | Resolved _ as r -> r
  | Root (s, _) as r -> begin
      match lookup_type_ident env s with
      | Type _ | Class _ | ClassType _ | Dot _ ->
        (* can't go from Root to any of these. *)
        assert false
      | Root _ -> r
      | Resolved (Identifier (CoreType _ | Type _) | Type _) as resolved ->
        resolved
      | Resolved (Identifier (Class _ | ClassType _) | Class _ | ClassType _) ->
        r
    end
  | Dot(r, s) -> Dot(lookup_label_parent env r, s)
  | Type(r, s) -> Type(lookup_signature env r, s)

let lookup_constructor env = function
  | Resolved _ as r -> r
  | Root (s, _) -> lookup_constructor_ident env s
  | Dot(r, s) -> Dot(lookup_label_parent env r, s)
  | Constructor(r, s) -> Constructor(lookup_datatype env r, s)
  | Extension(r, s) -> Extension(lookup_signature env r, s)
  | Exception(r, s) -> Exception(lookup_signature env r, s)

let lookup_field env = function
  | Resolved _ as r -> r
  | Root (s, _) -> lookup_field_ident env s
  | Dot(r, s) -> Dot(lookup_label_parent env r, s)
  | Field(r, s) -> Field(lookup_parent env r, s)

let lookup_extension env = function
  | Resolved _ as r -> r
  | Root (s, _) -> lookup_extension_ident env s
  | Dot(r, s) -> Dot(lookup_label_parent env r, s)
  | Extension(r, s) -> Extension(lookup_signature env r, s)
  | Exception(r, s) -> Exception(lookup_signature env r, s)

let lookup_exception env = function
  | Resolved _ as r -> r
  | Root (s, _) -> lookup_exception_ident env s
  | Dot(r, s) -> Dot(lookup_label_parent env r, s)
  | Exception(r, s) -> Exception(lookup_signature env r, s)

let lookup_value env = function
  | Resolved _ as r -> r
  | Root (s, _) -> lookup_value_ident env s
  | Dot(r, s) -> Dot(lookup_label_parent env r, s)
  | Value(r, s) -> Value(lookup_signature env r, s)

let lookup_class env = function
  | Resolved _ as r -> r
  | Root (s, _) -> lookup_class_ident env s
  | Dot(r, s) -> Dot(lookup_label_parent env r, s)
  | Class(r, s) -> Class(lookup_signature env r, s)

let lookup_class_type env = function
  | Resolved _ as r -> r
  | Root (s, _) -> lookup_class_type_ident env s
  | Dot(r, s) -> Dot(lookup_label_parent env r, s)
  | Class(r, s) -> Class(lookup_signature env r, s)
  | ClassType(r, s) -> ClassType(lookup_signature env r, s)

let lookup_method env = function
  | Resolved _ as r -> r
  | Root (s, _) -> lookup_method_ident env s
  | Dot(r, s) -> Dot(lookup_label_parent env r, s)
  | Method(r, s) -> Method(lookup_class_type env r, s)

let lookup_instance_variable env = function
  | Resolved _ as r -> r
  | Root (s, _) -> lookup_instance_variable_ident env s
  | Dot(r, s) -> Dot(lookup_label_parent env r, s)
  | InstanceVariable(r, s) -> InstanceVariable(lookup_class_type env r, s)

let lookup_label env = function
  | Resolved _ as r -> r
  | Root (s, _) -> lookup_label_ident env s
  | Dot(r, s) -> Dot(lookup_label_parent env r, s)
  | Label(r, s) -> Label(lookup_label_parent env r, s)

let lookup_element env = function
  | Resolved _ as r -> r
  | Root (s, TUnknown) -> Reference.any (lookup_element_ident env s)
  | Root (_, TPage) as r -> r (* there are no local pages. *)
  | Root (s, TModule) -> Reference.any (lookup_module_ident env s)
  | Root (s, TModuleType) -> Reference.any (lookup_module_type_ident env s)
  | Root (s, TType) -> Reference.any (lookup_type_ident env s)
  | Root (s, TConstructor) -> Reference.any (lookup_constructor_ident env s)
  | Root (s, TField) -> Reference.any (lookup_field_ident env s)
  | Root (s, TExtension) -> Reference.any (lookup_extension_ident env s)
  | Root (s, TException) -> Reference.any (lookup_exception_ident env s)
  | Root (s, TValue) -> Reference.any (lookup_value_ident env s)
  | Root (s, TClass) -> Reference.any (lookup_class_ident env s)
  | Root (s, TClassType) -> Reference.any (lookup_class_type_ident env s)
  | Root (s, TMethod) -> Reference.any (lookup_method_ident env s)
  | Root (s, TInstanceVariable) -> Reference.any (lookup_instance_variable_ident env s)
  | Root (s, TLabel) -> Reference.any (lookup_label_ident env s)
  | Dot(r, s) -> Dot(lookup_label_parent env r, s)
  | Module _ as r -> Reference.any @@ lookup_module env r
  | ModuleType _ as r -> Reference.any @@ lookup_module_type env r
  | Type _ as r -> Reference.any @@ lookup_type env r
  | Constructor _ as r -> Reference.any @@ lookup_constructor env r
  | Field _ as r -> Reference.any @@ lookup_field env r
  | Extension _ as r -> Reference.any @@ lookup_extension env r
  | Exception _ as r -> Reference.any @@ lookup_exception env r
  | Value _ as r -> Reference.any @@ lookup_value env r
  | Class _ as r -> Reference.any @@ lookup_class env r
  | ClassType _ as r -> Reference.any @@ lookup_class_type env r
  | Method _ as r -> Reference.any @@ lookup_method env r
  | InstanceVariable _ as r -> Reference.any @@ lookup_instance_variable env r
  | Label _ as r -> Reference.any @@ lookup_label env r


let lookup_section_title env lbl =
  match lbl with
  | Identifier id ->
    let lbl = Identifier.name id in
    begin match StringTbl.find lbl env.titles with
    | txt -> Some txt
    | exception Not_found -> None
    end
  | _ -> None
