(* Find *)
open Odoc_model.Names
open Component

type module_ = [ `FModule of ModuleName.t * Module.t ]

type module_type = [ `FModuleType of ModuleTypeName.t * ModuleType.t ]

type datatype = [ `FType of TypeName.t * TypeDecl.t ]

type class_ =
  [ `FClass of TypeName.t * Class.t | `FClassType of TypeName.t * ClassType.t ]

type value = [ `FValue of ValueName.t * Value.t ]

type label = [ `FLabel of Label.t ]

type exception_ = [ `FExn of ExceptionName.t * Exception.t ]

type extension = [ `FExt of Extension.t * Extension.Constructor.t ]

type substitution =
  [ `FModule_subst of ModuleSubstitution.t
  | `FType_subst of TypeDecl.t
  | `FModuleType_subst of ModuleTypeSubstitution.t ]

type signature = [ module_ | module_type ]

type type_ = [ datatype | class_ ]

type label_parent = [ signature | type_ ]

type constructor = [ `FConstructor of TypeDecl.Constructor.t ]

type polymorphic_constructor =
  [ `FPoly of TypeExpr.Polymorphic_variant.Constructor.t ]

type field = [ `FField of TypeDecl.Field.t ]

type any_in_type = [ constructor | field | polymorphic_constructor ]

type any_in_type_in_sig = [ `In_type of TypeName.t * TypeDecl.t * any_in_type ]

type any_in_sig =
  [ label_parent
  | value
  | label
  | exception_
  | extension
  | substitution
  | any_in_type_in_sig ]

type instance_variable =
  [ `FInstance_variable of InstanceVariableName.t * InstanceVariable.t ]

type method_ = [ `FMethod of MethodName.t * Method.t ]

type any_in_class_sig = [ instance_variable | method_ ]

(** Lookup by name, unambiguous *)

val module_in_sig : Signature.t -> ModuleName.t -> module_ option

val type_in_sig : Signature.t -> TypeName.t -> type_ option

val datatype_in_sig : Signature.t -> TypeName.t -> datatype option

val module_type_in_sig : Signature.t -> ModuleTypeName.t -> module_type option

val exception_in_sig : Signature.t -> ExceptionName.t -> exception_ option

val extension_in_sig : Signature.t -> ExtensionName.t -> extension option

val any_in_type : TypeDecl.t -> string -> any_in_type option

val any_in_typext : Extension.t -> string -> extension option

val value_in_sig : Signature.t -> ValueName.t -> value option

val method_in_class_signature :
  ClassSignature.t -> MethodName.t -> method_ option

val instance_variable_in_class_signature :
  ClassSignature.t -> InstanceVariableName.t -> instance_variable option

(** Maybe ambiguous *)

val class_in_sig : Signature.t -> TypeName.t -> class_ list

val signature_in_sig : Signature.t -> string -> signature list

val label_in_sig : Signature.t -> LabelName.t -> label list

val label_parent_in_sig : Signature.t -> string -> label_parent list

val any_in_sig : Signature.t -> string -> any_in_sig list

val any_in_type_in_sig : Signature.t -> string -> any_in_type_in_sig list

val any_in_class_signature : ClassSignature.t -> string -> any_in_class_sig list

(** Disambiguated lookups, returns the last match. *)

val class_in_sig_unambiguous : Signature.t -> TypeName.t -> class_ option

(** Lookup removed items *)

type removed_type =
  [ `FType_removed of TypeName.t * TypeExpr.t * TypeDecl.Equation.t ]

type careful_module = [ module_ | `FModule_removed of Cpath.module_ ]

type careful_module_type =
  [ module_type | `FModuleType_removed of ModuleType.expr ]

type core_type = [ `CoreType of TypeName.t ]

type careful_type = [ type_ | removed_type | core_type ]

type careful_class = [ class_ | removed_type ]

val careful_module_in_sig : Signature.t -> ModuleName.t -> careful_module option

val careful_module_type_in_sig :
  Signature.t -> ModuleTypeName.t -> careful_module_type option

val careful_type_in_sig : Signature.t -> TypeName.t -> careful_type option

val careful_class_in_sig : Signature.t -> TypeName.t -> careful_class option
