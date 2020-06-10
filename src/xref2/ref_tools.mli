(* Ref_tools *)
open Odoc_model.Paths
open Reference

type module_lookup_result =
  Resolved.Module.t * Cpath.Resolved.module_ * Component.Module.t

type module_type_lookup_result =
  Resolved.ModuleType.t * Cpath.Resolved.module_type * Component.ModuleType.t

type signature_lookup_result =
  Resolved.Signature.t * Cpath.Resolved.parent * Component.Signature.t

type datatype_lookup_result = Resolved.DataType.t * Component.TypeDecl.t

type class_lookup_result = Resolved.Class.t * Component.Class.t

type class_type_lookup_result = Resolved.ClassType.t * Component.ClassType.t

type type_lookup_result =
  [ `C of class_lookup_result
  | `CT of class_type_lookup_result
  | `T of datatype_lookup_result ]

type value_lookup_result = Resolved.Value.t

type label_parent_lookup_result =
  [ `C of class_lookup_result
  | `CT of class_type_lookup_result
  | `Page of Resolved.Page.t * (string * Identifier.Label.t) list
  | `S of signature_lookup_result
  | `T of datatype_lookup_result ]

type class_signature_lookup_result =
  Resolved.ClassSignature.t * Component.ClassSignature.t

val signature_lookup_result_of_label_parent :
  label_parent_lookup_result -> signature_lookup_result option

val class_lookup_result_of_type :
  type_lookup_result -> class_lookup_result option

val class_type_lookup_result_of_type :
  type_lookup_result -> class_type_lookup_result option

module Hashable : sig
  type t = bool * Resolved.Signature.t

  val equal : 'a -> 'a -> bool

  val hash : 'a -> int
end

module Memos1 : Hashtbl.S with type key = Hashable.t

module Hashable2 : sig
  type t = bool * Signature.t

  val equal : 'a -> 'a -> bool

  val hash : 'a -> int
end

module Memos2 : Hashtbl.S with type key = Hashable2.t


val module_lookup_to_signature_lookup :
  Env.t -> module_lookup_result -> signature_lookup_result option

val module_type_lookup_to_signature_lookup :
  Env.t -> module_type_lookup_result -> signature_lookup_result option

val type_lookup_to_class_signature_lookup :
  Env.t -> type_lookup_result -> class_signature_lookup_result option

module M : sig
  type t = module_lookup_result

  val of_component :
    Env.t ->
    Component.Module.t ->
    Cpath.Resolved.module_ ->
    Resolved.Module.t ->
    t

  val in_signature :
    Env.t ->
    signature_lookup_result ->
    Odoc_model.Names.ModuleName.t ->
    t option

  val of_element :
    Env.t ->
    [< `Module of Identifier.Module.t * Component.Module.t ] ->
    t option

  val in_env : Env.t -> Odoc_model.Names.UnitName.t -> t option
end

module MT : sig
  type t = module_type_lookup_result

  val of_component :
    'a ->
    Component.ModuleType.t ->
    Cpath.Resolved.module_type ->
    Resolved.ModuleType.t ->
    t

  val in_signature :
    'a ->
    signature_lookup_result ->
    Odoc_model.Names.ModuleTypeName.t ->
    t option

  val of_element :
    'a ->
    [< `ModuleType of Identifier.ModuleType.t * Component.ModuleType.t ] ->
    t option

  val in_env : Env.t -> Odoc_model.Names.UnitName.t -> t option
end

module CL : sig
  type t = class_lookup_result

  val of_element :
    'a -> [< `Class of Identifier.Class.t * Component.Class.t ] -> t

  val in_env : Env.t -> Odoc_model.Names.UnitName.t -> t option

  val of_component :
    'a ->
    Component.Class.t ->
    parent_ref:Resolved.Signature.t ->
    string ->
    t option
end

module CT : sig
  type t = class_type_lookup_result

  val of_element :
    'a ->
    [< `ClassType of [< Identifier.ClassType.t ] * Component.ClassType.t ] ->
    t

  val in_env : Env.t -> Odoc_model.Names.UnitName.t -> t option

  val of_component :
    'a ->
    Component.ClassType.t ->
    parent_ref:Resolved.Signature.t ->
    string ->
    t option
end

module DT : sig
  type t = datatype_lookup_result

  val of_component :
    'a ->
    Component.TypeDecl.t ->
    parent_ref:Resolved.Signature.t ->
    string ->
    t option

  val of_element :
    'a -> [< `Type of Identifier.DataType.t * Component.TypeDecl.t ] -> t

  val in_env : Env.t -> Odoc_model.Names.UnitName.t -> t option

  val in_signature :
    'a -> signature_lookup_result -> Odoc_model.Names.TypeName.t -> t option
end

module T : sig
  type t = type_lookup_result

  val in_env : Env.t -> Odoc_model.Names.UnitName.t -> type_lookup_result option

  val in_signature :
    'a -> signature_lookup_result -> string -> type_lookup_result option
end

module V : sig
  type t = value_lookup_result

  val in_env :
    Env.t -> Odoc_model.Names.UnitName.t -> value_lookup_result option

  val of_component :
    'a ->
    parent_ref:Resolved.Signature.t ->
    string ->
    value_lookup_result option

  val external_of_component :
    'a ->
    parent_ref:Resolved.Signature.t ->
    string ->
    value_lookup_result option

  val in_signature :
    'a ->
    signature_lookup_result ->
    Odoc_model.Names.ValueName.t ->
    value_lookup_result option
end

module L : sig
  type t = Resolved.Label.t

  val in_env : Env.t -> Odoc_model.Names.UnitName.t -> t option

  val in_page :
    'a -> [< `Page of 'b * ('c * Identifier.Label.t) list ] -> 'c -> t option

  val of_component :
    'a ->
    parent_ref:
      [< `Canonical of [< Resolved.Module.t ] * [< Module.t ]
      | `Class of [< Resolved.Signature.t ] * Odoc_model.Names.ClassName.t
      | `ClassType of
        [< Resolved.Signature.t ] * Odoc_model.Names.ClassTypeName.t
      | `Hidden of [< Resolved.Module.t ]
      | `Identifier of [< Identifier.LabelParent.t ]
      | `Module of [< Resolved.Signature.t ] * Odoc_model.Names.ModuleName.t
      | `ModuleType of
        [< Resolved.Signature.t ] * Odoc_model.Names.ModuleTypeName.t
      | `SubstAlias of [< Path.Resolved.Module.t ] * [< Resolved.Module.t ]
      | `Type of [< Resolved.Signature.t ] * Odoc_model.Names.TypeName.t ] ->
    string ->
    t option

  val in_label_parent :
    'a -> label_parent_lookup_result -> Odoc_model.Names.LabelName.t -> t option
end

module EC : sig
  type t = Resolved.Constructor.t

  val in_env : Env.t -> Odoc_model.Names.UnitName.t -> t option

  val of_component : 'a -> parent_ref:Resolved.Signature.t -> string -> t option

  val in_signature :
    'a ->
    signature_lookup_result ->
    Odoc_model.Names.ExtensionName.t ->
    t option
end

module EX : sig
  type t = Resolved.Exception.t

  val in_env : Env.t -> Odoc_model.Names.UnitName.t -> t option

  val of_component : 'a -> parent_ref:Resolved.Signature.t -> string -> t option

  val in_signature :
    'a ->
    signature_lookup_result ->
    Odoc_model.Names.ExceptionName.t ->
    t option
end

module CS : sig
  type t = Resolved.Constructor.t

  val in_env : Env.t -> Odoc_model.Names.UnitName.t -> t option

  val in_datatype :
    'a ->
    datatype_lookup_result ->
    Odoc_model.Names.ConstructorName.t ->
    t option

  val of_component : 'a -> Resolved.DataType.t -> string -> t option
end

module F : sig
  type t = Resolved.Field.t

  val in_env : Env.t -> Odoc_model.Names.UnitName.t -> t option

  val in_parent :
    'a -> label_parent_lookup_result -> Odoc_model.Names.FieldName.t -> t option

  val of_component : 'a -> Resolved.DataType.t -> string -> t option
end

module MM : sig
  type t = Resolved.Method.t

  val in_env : 'a -> 'b -> 'c option

  val in_class_signature :
    'a ->
    Resolved.ClassSignature.t * Component.ClassSignature.t ->
    Odoc_model.Names.MethodName.t ->
    t option

  val of_component : 'a -> Resolved.ClassSignature.t -> string -> t option
end

module MV : sig
  type t = Resolved.InstanceVariable.t

  val in_env : 'a -> 'b -> 'c option

  val in_class_signature :
    'a ->
    Resolved.ClassSignature.t * Component.ClassSignature.t ->
    Odoc_model.Names.InstanceVariableName.t ->
    t option

  val of_component : 'a -> Resolved.ClassSignature.t -> string -> t option
end

module LP : sig
  type t = label_parent_lookup_result

  val in_env : Env.t -> Odoc_model.Names.UnitName.t -> t option

  val in_signature : Env.t -> signature_lookup_result -> string -> t option
end

val resolve_label_parent_reference :
  Env.t -> LabelParent.t -> label_parent_lookup_result option

val resolve_signature_reference :
  Env.t -> Signature.t -> signature_lookup_result option

val resolve_datatype_reference :
  Env.t -> DataType.t -> datatype_lookup_result option

val resolve_module_reference : Env.t -> Module.t -> module_lookup_result option

val resolve_class_signature_reference :
  Env.t -> ClassSignature.t -> class_signature_lookup_result option

val resolved1 :
  [< `Canonical of [< Resolved.Module.t ] * [< Module.t ]
  | `Class of [< Resolved.Signature.t ] * Odoc_model.Names.ClassName.t
  | `ClassType of [< Resolved.Signature.t ] * Odoc_model.Names.ClassTypeName.t
  | `Constructor of
    [< Resolved.DataType.t ] * Odoc_model.Names.ConstructorName.t
  | `Exception of [< Resolved.Signature.t ] * Odoc_model.Names.ExceptionName.t
  | `Extension of [< Resolved.Signature.t ] * Odoc_model.Names.ExtensionName.t
  | `Field of [< Resolved.Parent.t ] * Odoc_model.Names.FieldName.t
  | `Hidden of [< Resolved.Module.t ]
  | `Identifier of [< Identifier.t ]
  | `InstanceVariable of
    [< Resolved.ClassSignature.t ] * Odoc_model.Names.InstanceVariableName.t
  | `Label of [< Resolved.LabelParent.t ] * Odoc_model.Names.LabelName.t
  | `Method of [< Resolved.ClassSignature.t ] * Odoc_model.Names.MethodName.t
  | `Module of [< Resolved.Signature.t ] * Odoc_model.Names.ModuleName.t
  | `ModuleType of [< Resolved.Signature.t ] * Odoc_model.Names.ModuleTypeName.t
  | `SubstAlias of [< Path.Resolved.Module.t ] * [< Resolved.Module.t ]
  | `Type of [< Resolved.Signature.t ] * Odoc_model.Names.TypeName.t
  | `Value of [< Resolved.Signature.t ] * Odoc_model.Names.ValueName.t ] ->
  Resolved.t option

val resolved3 :
  [< `Canonical of [< Resolved.Module.t ] * [< Module.t ]
  | `Class of [< Resolved.Signature.t ] * Odoc_model.Names.ClassName.t
  | `ClassType of [< Resolved.Signature.t ] * Odoc_model.Names.ClassTypeName.t
  | `Constructor of
    [< Resolved.DataType.t ] * Odoc_model.Names.ConstructorName.t
  | `Exception of [< Resolved.Signature.t ] * Odoc_model.Names.ExceptionName.t
  | `Extension of [< Resolved.Signature.t ] * Odoc_model.Names.ExtensionName.t
  | `Field of [< Resolved.Parent.t ] * Odoc_model.Names.FieldName.t
  | `Hidden of [< Resolved.Module.t ]
  | `Identifier of [< Identifier.t ]
  | `InstanceVariable of
    [< Resolved.ClassSignature.t ] * Odoc_model.Names.InstanceVariableName.t
  | `Label of [< Resolved.LabelParent.t ] * Odoc_model.Names.LabelName.t
  | `Method of [< Resolved.ClassSignature.t ] * Odoc_model.Names.MethodName.t
  | `Module of [< Resolved.Signature.t ] * Odoc_model.Names.ModuleName.t
  | `ModuleType of [< Resolved.Signature.t ] * Odoc_model.Names.ModuleTypeName.t
  | `SubstAlias of [< Path.Resolved.Module.t ] * [< Resolved.Module.t ]
  | `Type of [< Resolved.Signature.t ] * Odoc_model.Names.TypeName.t
  | `Value of [< Resolved.Signature.t ] * Odoc_model.Names.ValueName.t ]
  * 'a
  * 'b ->
  Resolved.t option

val resolved2 :
  [< `Canonical of [< Resolved.Module.t ] * [< Module.t ]
  | `Class of [< Resolved.Signature.t ] * Odoc_model.Names.ClassName.t
  | `ClassType of [< Resolved.Signature.t ] * Odoc_model.Names.ClassTypeName.t
  | `Constructor of
    [< Resolved.DataType.t ] * Odoc_model.Names.ConstructorName.t
  | `Exception of [< Resolved.Signature.t ] * Odoc_model.Names.ExceptionName.t
  | `Extension of [< Resolved.Signature.t ] * Odoc_model.Names.ExtensionName.t
  | `Field of [< Resolved.Parent.t ] * Odoc_model.Names.FieldName.t
  | `Hidden of [< Resolved.Module.t ]
  | `Identifier of [< Identifier.t ]
  | `InstanceVariable of
    [< Resolved.ClassSignature.t ] * Odoc_model.Names.InstanceVariableName.t
  | `Label of [< Resolved.LabelParent.t ] * Odoc_model.Names.LabelName.t
  | `Method of [< Resolved.ClassSignature.t ] * Odoc_model.Names.MethodName.t
  | `Module of [< Resolved.Signature.t ] * Odoc_model.Names.ModuleName.t
  | `ModuleType of [< Resolved.Signature.t ] * Odoc_model.Names.ModuleTypeName.t
  | `SubstAlias of [< Path.Resolved.Module.t ] * [< Resolved.Module.t ]
  | `Type of [< Resolved.Signature.t ] * Odoc_model.Names.TypeName.t
  | `Value of [< Resolved.Signature.t ] * Odoc_model.Names.ValueName.t ]
  * 'a ->
  Resolved.t option

val resolve_reference_dot_sg :
  Env.t ->
  parent_path:Cpath.Resolved.parent ->
  parent_ref:Resolved.Signature.t ->
  parent_sg:Component.Signature.t ->
  string ->
  Resolved.t option

val resolve_reference_dot_page :
  'a ->
  [< `Page of 'b * ('c * Identifier.Label.t) list ] ->
  'c ->
  Resolved.t option

val resolve_reference_dot_type :
  'a ->
  parent_ref:Resolved.DataType.t ->
  Component.TypeDecl.t ->
  string ->
  Resolved.t option

val resolve_reference_dot_class :
  Env.t -> type_lookup_result -> string -> Resolved.t option

val resolve_reference_dot :
  Env.t -> LabelParent.t -> string -> Resolved.t option

val resolve_reference : Env.t -> t -> Resolved.t option
